module UI.Constants (
  app
)

where
import           Brick     (App (App, appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent))
import           UI.Attrs  (attrMap)
import qualified UI.Events as UE (appStartEvent)
import           UI.Events (eventHandler)
import           UI.Types  (AppState, Events)
import qualified UI.Utils  as UU (appChooseCursor)
import           UI.Views  (mkView)

app :: App AppState Events String
app = App {
        appDraw = mkView,
        appChooseCursor = UU.appChooseCursor,
        appHandleEvent = eventHandler ,
        appStartEvent = UE.appStartEvent,
        appAttrMap = const attrMap
          }
