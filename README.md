# Recurse
A simple, small extension of Gloss useful for making recursive art.

## To Use
Use the given functions for making a list of recursive pictures and then feed them into the drawRecurse function with a background color.

## Example
```haskell
module Main where
  import Recurse
  import Graphics.Gloss
  
  myPicture :: [Picture]
  myPicture = recurseRect 1.0 0 0 90 red (FourCorners 0)
  
  main :: IO ()
  main = drawRecurse myPicture black
```
Should make this:
![Red Rect FourCorners](https://github.com/HenningTonko/Recurse/blob/master/examples/FourCornersRedRect.png)
