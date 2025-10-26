module UI.Attrs 
  (cursorAttr
  , attrMap
  , selectedAttr)
where
import Brick (AttrName, attrName, AttrMap)
import Graphics.Vty (defAttr, reverseVideo, withStyle, green, withForeColor)
import qualified Brick.AttrMap as BA (attrMap)

attrMap :: AttrMap
attrMap = BA.attrMap defAttr [(cursorAttr, defAttr `withStyle` reverseVideo),
                              (selectedAttr, defAttr `withForeColor` green)]

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

selectedAttr :: AttrName
selectedAttr = attrName "selected"
