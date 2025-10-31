module UI.Attrs
  (cursorAttr
  , attrMap
  , selectedAttr)
where
import           Brick                     (AttrMap, AttrName, attrName)
import qualified Brick.AttrMap             as BA (attrMap)
import           Brick.Widgets.Dialog      (buttonSelectedAttr)
import           Brick.Widgets.ProgressBar (progressCompleteAttr)
import           Graphics.Vty              (defAttr, green, reverseVideo,
                                            withBackColor, withForeColor,
                                            withStyle)

attrMap :: AttrMap
attrMap = BA.attrMap defAttr [(cursorAttr, defAttr `withStyle` reverseVideo),
                              (selectedAttr, defAttr `withForeColor` green),
                              (buttonSelectedAttr, defAttr `withStyle` reverseVideo),
                              (progressCompleteAttr, defAttr `withBackColor` green)
                             ]

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

selectedAttr :: AttrName
selectedAttr = attrName "selected"
