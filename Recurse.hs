module Recurse where
  import Graphics.Gloss
  import Graphics.Gloss.Data.ViewPort

  data DrawInstruct = SmallToBig | BigToSmall | FourCorners Bool deriving Eq

  recurseCircle :: Float -> Float -> Float -> Color -> DrawInstruct -> [Picture]
  recurseCircle x y r c di
    | r < 0.05 = [translate 0 0 (color black (circle r))]
    | di == BigToSmall = [translate x y (color c (circle r))] ++ recurseCircle (x+45) y (r * 0.9) c di
    | di == SmallToBig = [translate x y (color c (circle r))] ++ recurseCircle (x-45) y (r * 0.9) c di
    | di == (FourCorners True) = [translate x y (color c (circle r))] ++ recurseCircle (x-45) (y-45) (r * 0.9) c (FourCorners False) ++ recurseCircle (x-45) (y+45) (r*0.9) c (FourCorners False) ++ recurseCircle (x+45) (y-45) (r*0.9) c (FourCorners False) ++ recurseCircle (x+45) (y+45) (r*0.9) c (FourCorners False)
    | di == (FourCorners False) = [translate (x-45) (y-45) $ color c $ circle r]
    | otherwise = []

  recurseRect :: Float -> Float -> Float -> Color -> DrawInstruct -> [Picture]
  recurseRect x y w c di
    | w < 0.05 = [translate 0 0 (color black (rectangleWire w w))]
    | otherwise = [translate x y (color c (rectangleWire (w*2) w))] ++ recurseRect x (if di == SmallToBig then y+45 else y-45) (w * 0.9) c di

  drawRecurse :: Picture -> Color -> IO ()
  drawRecurse p c = display FullScreen black p

  {- import Graphics.Gloss
  import Graphics.Gloss.Data.ViewPort

  data DrawInstruct = SmallToBig | BigToSmall | FourCorners deriving (Show, Eq)

  data Direction = TopToBottom | BottomToTop | LeftToRight | RightToLeft | Nothing deriving (Eq, Show)

  data Shape = Rect Point Float Float Color | Circ Point Float Float Color deriving (Eq, Show)

  class ShapeClass a where
    center :: a -> Point
    top :: a -> Float
    bottom :: a -> Float
    left :: a -> Float
    right :: a -> Float
    width :: a -> Float
    height :: a -> Float

  instance ShapeClass Shape where
    center (Rect c _ _ _) = c
    top (Rect (_, y) _ h _) = y+h
    bottom (Rect (_, y) _ h _) = y-h
    left (Rect (x, _) w _ _) = x-w
    right (Rect (x, _) w _ _) = x+w
    width (Rect _ w _ _) = w
    height (Rect _ _ h _) = h

  createPicture :: DrawInstruct -> Shape -> Direction -> [Picture]
  createPicture SmallToBig (Rect (x, y) w h c) di = [translate x y $ color c $ rectangleWire w h] ++ (if w > 0.01 then createPicture SmallToBig (Rect ((if di == LeftToRight then x+30 else x-30), y) (w*0.9) (h*0.9) c) di else [])
  createPicture _ _ _ = [translate 0 0 $ color red $ polygon [(0, 0), (0+0, 0), (0, 0), (0, 0)]]

  drawRecurse :: Picture -> IO ()
  drawRecurse p = display FullScreen black p -}