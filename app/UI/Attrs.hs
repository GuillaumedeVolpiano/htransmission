module UI.Attrs
  (cursorAttr
  , attrMap
  , selectedAttr)
where
import           Brick                     (AttrMap, AttrName, attrName, on)
import qualified Brick.AttrMap             as BA (attrMap)
import           Brick.Widgets.Dialog      (buttonSelectedAttr)
import           Brick.Widgets.ProgressBar (progressCompleteAttr)
import           Graphics.Vty              (defAttr, green, reverseVideo,
                                            withBackColor, withForeColor,
                                            withStyle, yellow, black, white, red)
import Brick.Widgets.FileBrowser (fileBrowserSelectedAttr)
import Brick.Forms (focusedFormInputAttr, invalidFormInputAttr)

attrMap :: AttrMap
attrMap = BA.attrMap defAttr [(cursorAttr, defAttr `withStyle` reverseVideo),
                              (selectedAttr, defAttr `withForeColor` green),
                              (buttonSelectedAttr, defAttr `withStyle` reverseVideo),
                              (progressCompleteAttr, defAttr `withBackColor` green),
                              (fileBrowserSelectedAttr, defAttr `withStyle` reverseVideo),
                              (focusedFormInputAttr, black `on` yellow),
                              (invalidFormInputAttr, white `on` red)
                             ]

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

selectedAttr :: AttrName
selectedAttr = attrName "selected"
