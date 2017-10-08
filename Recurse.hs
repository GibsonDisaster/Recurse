module Recurse where
  import Graphics.Gloss
  import Graphics.Gloss.Data.ViewPort

  data DrawInstruct = SmallToBig | BigToSmall deriving Eq

  recurseCircle :: Float -> Float -> Float -> Color -> DrawInstruct -> [Picture]
  recurseCircle x y r c di
    | r < 0.05 = [translate 0 0 (color black (circle r))]
    | otherwise = [translate x y (color c (circle r))] ++ recurseCircle (if di == BigToSmall then x+45 else x-45) y (r * 0.9) c di

  recurseRect :: Float -> Float -> Float -> Color -> DrawInstruct -> [Picture]
  recurseRect x y w c di
    | w < 0.05 = [translate 0 0 (color black (rectangleWire w w))]
    | otherwise = [translate x y (color c (rectangleWire (w*2) w))] ++ recurseRect x (if di == SmallToBig then y+45 else y-45) (w * 0.9) c di

  drawRecurse :: Picture -> Color -> IO ()
  drawRecurse p c = display FullScreen c p