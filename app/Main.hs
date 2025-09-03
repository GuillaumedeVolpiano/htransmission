{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Effectful               (runEff)
import           Effectful.Reader.Static (runReader)
import           Effectful.Wreq          (runWreq)
import Transmission.RPC.Client (fromUrl, getTorrent)
import           Options.Applicative     (Parser, execParser, fullDesc,
                                          help, helper, info, long, metavar, progDesc, short, strOption,
                                          value, (<**>))
import Effectful.Log (runLog, LogLevel (LogTrace))
import Log.Backend.StandardOutput (withStdOutLogger)
import Effectful.Time (runTime)
import qualified Data.Text as T (pack)
import Transmission.RPC.Types (ID(ID))

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

  result <- runEff . runWreq $ do 
                                  client <- fromUrl url Nothing Nothing
                                  withStdOutLogger $ \stdoutLogger -> do
                                    runReader client . runLog (T.pack "htransmission") stdoutLogger LogTrace . runTime $ getTorrent (ID 1) Nothing Nothing
  print result
