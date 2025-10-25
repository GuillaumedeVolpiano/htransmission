module UI.Attrs 
  (cursorAttr
  , attrMap)
where
import Brick (AttrName, attrName, AttrMap)
import Graphics.Vty (defAttr, reverseVideo, withStyle, withBackColor, green)
import qualified Brick.AttrMap as BA (attrMap)

attrMap :: AttrMap
attrMap = BA.attrMap defAttr [(cursorAttr, defAttr `withStyle` reverseVideo),
                              (selectedAttr, defAttr `withBackColor` green)]

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

selectedAttr :: AttrName
selectedAttr = attrName "selected"
