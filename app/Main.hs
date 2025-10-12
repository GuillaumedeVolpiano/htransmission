{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text                  as T (pack)
import           Effectful                  (runEff)
import           Effectful.Log              (LogLevel (LogTrace), runLog)
import           Effectful.Prim.IORef       (runPrim)
import           Effectful.Reader.Static    (runReader)
import           Effectful.Time             (runTime)
import           Effectful.Wreq             (runWreq)
import           Log.Backend.StandardOutput (withStdOutLogger)
import           Options.Applicative        (Parser, execParser, fullDesc, help,
                                             helper, info, long, metavar,
                                             progDesc, short, strOption, value,
                                             (<**>))
import           Transmission.RPC.Client    (fromUrl, sessionStats)

newtype Args = Args {
                 getHost :: String
                 }

args :: Parser Args
args =
  Args <$> strOption
             (long "host" <> short 'h' <> metavar "Host" <> value "http://localhost:9091/transmission/rpc" <> help "link to the transmission host, like http://username:password@host:9091/transmission/rpc")

main :: IO ()
main = do
  (Args url) <- execParser . info (args <**> helper) $ (fullDesc <> progDesc "A command line to communicate with transmission-daemon")

  result <- runEff . runWreq . runPrim $ do
                                  client <- fromUrl url Nothing Nothing
                                  withStdOutLogger $ \stdoutLogger -> do
                                    runReader client . runLog (T.pack "htransmission") stdoutLogger LogTrace . runTime $ sessionStats Nothing
  print result
