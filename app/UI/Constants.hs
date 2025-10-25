module UI.Constants (
  app
)  

where
import Brick (App (appDraw, appChooseCursor, appHandleEvent, appStartEvent, appAttrMap, App))
import UI.Types (AppState, Events)
import UI.Views (mkView)
import UI.Events (eventHandler, updateView)
import qualified UI.Utils as UU (appChooseCursor)
import UI.Attrs (attrMap)

app :: App AppState Events String 
app = App {
        appDraw = mkView,
        appChooseCursor = UU.appChooseCursor,
        appHandleEvent = eventHandler ,
        appStartEvent = updateView True,
        appAttrMap = const attrMap 
          }
