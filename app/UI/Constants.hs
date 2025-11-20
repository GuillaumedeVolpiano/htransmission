{-# LANGUAGE OverloadedStrings #-}
module UI.Constants (
  app
, addForm
, fileBrowser)

where
import           Brick     (App (App, appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent), (<+>), str)
import           Types     (AppState, Events, AddTorrent(AddTorrent), tagsL, destinationL, completedL, startTorrentL)
import           UI.Attrs  (attrMap)
import qualified UI.Events as UE (appStartEvent)
import           UI.Events (eventHandler)
import qualified UI.Utils  as UU (appChooseCursor)
import           UI.Views  (mkView)
import Brick.Forms (Form, newForm, editTextField, checkboxField, (@@=))
import Brick.Widgets.FileBrowser (FileBrowser, setFileBrowserEntryFilter, fileExtensionMatch, newFileBrowser, selectNonDirectories)

app :: App AppState Events String
app = App {
        appDraw = mkView,
        appChooseCursor = UU.appChooseCursor,
        appHandleEvent = eventHandler ,
        appStartEvent = UE.appStartEvent,
        appAttrMap = const attrMap
          }

addForm :: Form AddTorrent Events String
addForm = newForm [ (str "Tags: " <+>) @@= editTextField tagsL "Tags" (Just 1)
                  , (str "Destination folder: " <+>) @@= editTextField destinationL "Destination" (Just 1)
                  , (str "Completed: " <+>) @@= checkboxField completedL "Completed" "Completed" 
                  , (str "Start torrent at launch " <+>) @@= checkboxField startTorrentL "Start torrent" 
                      "Start torrent"
                  ] (AddTorrent "" "/var/lib/rtorrent/downloads" False True)


fileBrowser :: IO (FileBrowser String)
fileBrowser = setFileBrowserEntryFilter (Just (fileExtensionMatch "torrent")) 
  <$> newFileBrowser selectNonDirectories "FileBrowser" Nothing
