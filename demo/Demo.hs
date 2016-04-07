import Diagrams.Prelude as D
import Diagrams.Backend.Cairo
import Diagrams.Backend.WX

import Graphics.UI.WX as W hiding (circle, (#))
import qualified Graphics.UI.WX.Types as W

main :: IO ()
main = W.start $ do
  f <- W.frame [W.text := "diagrams-wx demo"]
  p <- panel f [on paint := paintDiagram]

  W.set f [ fullRepaintOnResize := False
          , layout := minsize (sz 300 300) $ fill $ widget p
          ]

paintDiagram :: DC a -> Rect -> IO ()
paintDiagram dc rect = drawDiagram dc sampleDiagram pt rs W.white []
  where
    pt = W.Point (rectLeft rect) (rectTop rect)
    rs = dims2D (fromIntegral $ rectWidth rect) (fromIntegral $ rectHeight rect)

sampleDiagram :: QDiagram Cairo V2 Double Any
sampleDiagram = mconcat
  [ circle 0.1 # fc D.green
  , triangle 1 # scale 0.4 # fc D.yellow
  , square 1   # fc D.blue
  , circle 1   # fc D.red
  ]

