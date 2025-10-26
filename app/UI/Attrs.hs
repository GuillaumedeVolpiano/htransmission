module UI.Attrs 
  (cursorAttr
  , attrMap
  , selectedAttr)
where
import Brick (AttrName, attrName, AttrMap)
import Graphics.Vty (defAttr, reverseVideo, withStyle, green, withForeColor)
import qualified Brick.AttrMap as BA (attrMap)
import Brick.Widgets.Dialog (buttonSelectedAttr)

attrMap :: AttrMap
attrMap = BA.attrMap defAttr [(cursorAttr, defAttr `withStyle` reverseVideo),
                              (selectedAttr, defAttr `withForeColor` green),
                              (buttonSelectedAttr, defAttr `withStyle` reverseVideo)
                             ]

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

selectedAttr :: AttrName
selectedAttr = attrName "selected"
