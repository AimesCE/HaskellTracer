module Lib
    ( someFunc
    ) where

import System.IO
import Data.Char
import Geo

color (Ray _ direction) =
    ((Vec3 1.0 1.0 1.0) `scale` (1.0 - t)) + ((Vec3 0.5 0.7 1.0) `scale` t)
        where
            unit_Direction = makeUnit direction
            t = 0.5 * ((getY unit_Direction) + 1.0)

showRound = show . round

--scaleRGB :: Num a => a -> a -> (a, a) -> [Char]
scaleRGB nx ny (x, y) = (showRound ir) ++ " " ++ (showRound ig) ++ " " ++ (showRound ib) ++ "\n"
    where
        lower_left_corner = Vec3 (-2.0) (-1.0) (-1.0)
        horizontal = Vec3 4.0 0.0 0.0
        vertical = Vec3 0.0 2.0 0.0
        origin = Vec3 0.0 0.0 0.0
        u = (x / nx)
        v = (y / ny)
        ray = Ray origin (lower_left_corner + (horizontal `scale` u) + (vertical `scale` v))
        col = color ray
        ir = 255.99 * (getX col)
        ig = 255.99 * (getY col)
        ib  = 255.99 * (getZ col)

someFunc :: IO ()
someFunc = do
    let nx = 1000
    let ny = 500
    let zeroThroughNX = [0..(nx - 1)]
    let nyDownToZero = reverse [0..(ny - 1)]
    let pixelCoordinates = [(x, y) | y <- nyDownToZero, x <- zeroThroughNX]
    let header = "P3\n" ++ show (truncate nx) ++ " " ++ show (truncate ny) ++ "\n255\n"
    let scaledPixelCoordinates = map (scaleRGB nx ny) pixelCoordinates
    let fullFileContents = header ++ (scaledPixelCoordinates >>= (\x -> x))
    writeFile "FirstImage.ppm" fullFileContents