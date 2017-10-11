module Recurse where
  import Graphics.Gloss
  import Graphics.Gloss.Data.ViewPort

  -- TODO Make triangles draw from center origin, not bottom left point.

  data DrawInstruct = SmallToBig | BigToSmall | FourCorners Int deriving Eq

  recurseCircle :: Float -> Float -> Float -> Float -> Color -> DrawInstruct -> [Picture]
  recurseCircle spacing x y r c di
    | r < 0.01 = []
    | di == BigToSmall = [translate x y (color c (circle r))] ++ recurseCircle spacing (x+spacing) y (r * 0.9) c di
    | di == SmallToBig = [translate x y (color c (circle r))] ++ recurseCircle spacing (x-spacing) y (r * 0.9) c di
    | di == (FourCorners 0) = [translate (x) (y) $ color c $ circle r] ++ recurseCircle spacing (x-spacing) (y+spacing) (0.8 * r) c (FourCorners 1) ++ recurseCircle spacing (x+spacing) (y+spacing) (0.8 * r) c (FourCorners 2) ++ recurseCircle spacing (x+spacing) (y-spacing) (0.8 * r) c (FourCorners 3) ++ recurseCircle spacing (x-spacing) (y-spacing) (0.8 * r) c (FourCorners 4)
    | di == (FourCorners 1) = [translate x y $ color c $ circle r] ++ recurseCircle spacing (x-spacing) (y+spacing) (0.7 * r) c (FourCorners 1)
    | di == (FourCorners 2) = [translate x y $ color c $ circle r] ++ recurseCircle spacing (x+spacing) (y+spacing) (0.7 * r) c (FourCorners 2)
    | di == (FourCorners 3) = [translate x y $ color c $ circle r] ++ recurseCircle spacing (x+spacing) (y-spacing) (0.7 * r) c (FourCorners 3)
    | di == (FourCorners 4) = [translate x y $ color c $ circle r] ++ recurseCircle spacing (x-spacing) (y-spacing) (0.7 * r) c (FourCorners 4)
    | otherwise = []

  recurseRect :: Float -> Float -> Float -> Float -> Color -> DrawInstruct -> [Picture]
  recurseRect spacing x y w c di
    | w < 0.01 = []
    | di == BigToSmall = [translate x y (color c (rectangleWire w w))] ++ recurseRect spacing (x+spacing) y (w * 0.9) c di
    | di == SmallToBig = [translate x y (color c (rectangleWire w w))] ++ recurseRect spacing (x-spacing) y (w * 0.9) c di
    | di == (FourCorners 0) = [translate (x) (y) $ color c $ rectangleWire w w] ++ recurseRect spacing (x-spacing) (y+spacing) (0.8 * w) c (FourCorners 1) ++ recurseRect spacing (x+spacing) (y+spacing) (0.8 * w) c (FourCorners 2) ++ recurseRect spacing (x+spacing) (y-spacing) (0.8 * w) c (FourCorners 3) ++ recurseRect spacing (x-spacing) (y-spacing) (0.8 * w) c (FourCorners 4)
    | di == (FourCorners 1) = [translate x y $ color c $ rectangleWire w w] ++ recurseRect spacing (x-spacing) (y+spacing) (0.7 * w) c (FourCorners 1)
    | di == (FourCorners 2) = [translate x y $ color c $ rectangleWire w w] ++ recurseRect spacing (x+spacing) (y+spacing) (0.7 * w) c (FourCorners 2)
    | di == (FourCorners 3) = [translate x y $ color c $ rectangleWire w w] ++ recurseRect spacing (x+spacing) (y-spacing) (0.7 * w) c (FourCorners 3)
    | di == (FourCorners 4) = [translate x y $ color c $ rectangleWire w w] ++ recurseRect spacing (x-spacing) (y-spacing) (0.7 * w) c (FourCorners 4)
    | otherwise = []

  recurseTriangle :: Float -> Float -> Float -> Float -> Color -> DrawInstruct -> [Picture]
  recurseTriangle spacing x y w c di
    | w < 0.01 = []
    | di == BigToSmall = [translate x y (color c (line [(x, y), (x+w, y), (x+(w/2), y+w), (x, y)]))] ++ recurseTriangle spacing (x+spacing) y (w * 0.9) c di
    | di == SmallToBig = [translate x y (color c (line [(x, y), (x+w, y), (x+(w/2), y+w), (x, y)]))] ++ recurseTriangle spacing (x-spacing) y (w * 0.9) c di
    | di == (FourCorners 0) = [translate x y $ color c $ line [(x, y), (x+w, y), (x+(w/2), y+w), (x, y)]] ++ recurseTriangle spacing (x-spacing) (y+spacing) (0.8 * w) c (FourCorners 1) ++ recurseTriangle spacing (x+spacing) (y+spacing) (0.8 * w) c (FourCorners 2) ++ recurseTriangle spacing (x+spacing) (y-spacing) (0.8 * w) c (FourCorners 3) ++ recurseTriangle spacing (x-spacing) (y-spacing) (0.8 * w) c (FourCorners 4)
    | di == (FourCorners 1) = [translate x y $ color c $ line [(x, y), (x+w, y), (x+(w/2), y+w), (x, y)]] ++ recurseTriangle spacing (x-spacing) (y+spacing) (0.7 * w) c (FourCorners 1)
    | di == (FourCorners 2) = [translate x y $ color c $ line [(x, y), (x+w, y), (x+(w/2), y+w), (x, y)]] ++ recurseTriangle spacing (x+spacing) (y+spacing) (0.7 * w) c (FourCorners 2)
    | di == (FourCorners 3) = [translate x y $ color c $ line [(x, y), (x+w, y), (x+(w/2), y+w), (x, y)]] ++ recurseTriangle spacing (x+spacing) (y-spacing) (0.7 * w) c (FourCorners 3)
    | di == (FourCorners 4) = [translate x y $ color c $ line [(x, y), (x+w, y), (x+(w/2), y+w), (x, y)]] ++ recurseTriangle spacing (x-spacing) (y-spacing) (0.7 * w) c (FourCorners 4)
    | otherwise = []

  drawRecurse :: [Picture] -> Color -> IO ()
  drawRecurse p c = display FullScreen c (Pictures p)