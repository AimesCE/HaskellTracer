module Geo
    ( Vec3 (Vec3)
    , RGB
    , getX
    , getY
    , getZ
    , dot
    , cross
    , scale
    , vDivide
    , vAdd
    , vSubtract

    , squaredLength
    , vLen
    , makeUnit
    , Ray (Ray)
    ) where

data Vec3 = Vec3 Float Float Float

instance Num Vec3 where 
    (+) (Vec3 a b c) (Vec3 d e f) = Vec3 (a + d) (b + e) (c + f)
    (-) (Vec3 a b c) (Vec3 d e f) = Vec3 (a - d) (b - e) (c - f)
    (*) (Vec3 a b c) (Vec3 d e f) = Vec3 (a * d) (b * e) (c * f)
    negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
    abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
    signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
    fromInteger x = Vec3 a a a
        where
            a = fromInteger x

getX (Vec3 x _ _) = x
getY (Vec3 _ y _) = y
getZ (Vec3 _ _n z) = z

dot (Vec3 a b c) (Vec3 d e f) = (a * d) + (b * e) + (c * f)
cross (Vec3 a b c) (Vec3 d e f) = Vec3 ((b * f) - (f * b)) (-(a * f) - (c * d)) ((a * e) - (b * d))
scale (Vec3 a b c) s = Vec3 (a * s) (b * s) (c * s)
vDivide (Vec3 a b c) t = Vec3 (a * k) (b * k) (c * k)
            where 
                k = 1.0 / t
vAdd (Vec3 a b c) toAdd = Vec3 (a + toAdd) (b + toAdd) (c + toAdd)
vSubtract (Vec3 a b c) toSub = Vec3 (a - toSub) (b - toSub) (c - toSub)

squaredLength vec = vec `dot` vec
vLen vec = sqrt $ squaredLength vec

makeUnit vec = vDivide vec $ vLen vec


type RGB = Vec3
type Origin = Vec3
type Direction = Vec3


data Ray = Ray Origin Direction

pointAtParameter (Ray a b) t = (+) a $ scale b t
