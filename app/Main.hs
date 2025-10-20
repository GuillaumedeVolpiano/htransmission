{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Brick                      (App (..), customMain,
                                             showFirstCursor)
import           Brick.AttrMap              (attrMap)
import           Brick.BChan                (newBChan)
import qualified Data.Text.IO               as T (putStr)
import           Effectful                  (runEff)
import           Effectful.Prim.IORef       (runPrim)
import           Effectful.Wreq             (runWreq)
import           Graphics.Vty               (defAttr, defaultConfig)
import           Graphics.Vty.Platform.Unix (mkVty)
import           Options.Applicative        (Parser, execParser, fullDesc, help,
                                             helper, info, long, metavar,
                                             progDesc, short, strOption, value,
                                             (<**>))
import           Prelude                    hiding (log)
import           Transmission.RPC.Client    (fromUrl)
import           Types                      (AppState (AppState), Events,
                                             View (Main), log)
import           UI.Events                  (eventHandler, getElements,
                                             startTimer)
import           UI.Views                   (mkView)
import UI.KeyEvents (keyConfig, dispatcher)

newtype Args = Args {
                 getHost :: String
                 }

app :: App AppState Events Int
app = App {
        appDraw = mkView,
        appChooseCursor = showFirstCursor,
        appHandleEvent = eventHandler ,
        appStartEvent = pure (),
        appAttrMap = const $ attrMap defAttr []
          }

args :: Parser Args
args =
  Args <$> strOption
             (long "host" <> short 'h' <> metavar "Host" <> value "http://localhost:9091/transmission/rpc" <> help "link to the transmission host, like http://username:password@host:9091/transmission/rpc")

main :: IO ()
main = do
  (Args url) <- execParser . info (args <**> helper) $ (fullDesc <> progDesc "A command line to communicate with transmission-daemon")
  chan <- newBChan 10
  startTimer chan
  vty <- mkVty defaultConfig
  client <- runEff . runWreq . runPrim $ fromUrl url Nothing Nothing
  (getElemsLog, (torrents, sesh, seshStats)) <- getElements Main client
  let appState = AppState Main client torrents sesh seshStats [getElemsLog] keyConfig dispatcher
  finalState <- customMain vty (mkVty defaultConfig) (Just chan) app appState
  mapM_ T.putStr . reverse $ log finalState
