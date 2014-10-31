module Functions where

import Graphics.Gloss
import Graphics.Gloss.Geometry

infixl 7 .*, .*., ./
infixl 6 .+., .-.

--Multiply a point and a float
(.*) :: Point -> Float -> Point
(x, y) .* f = (x * f, y * f)

--Multiply two points
(.*.) :: Point -> Point -> Point
(x, y) .*. (z, w) = (x * z, y * w)

--Divide a point by a float
(./) :: Point -> Float -> Point
(x, y) ./ f = (x / f, y / f)

--Add two points
(.+.) :: Point -> Point -> Point
(x, y) .+. (z, w) = (x + z, y + w)

--Substract two points
(.-.) :: Point -> Point -> Point
(x, y) .-. (z, w) = (x - z, y - w)

--Get the distance between two points
(.<>.) :: Point -> Point -> Float
(x, y) .<>. (z, w) = sqrt $ (x - z) ** 2 + (y - w) ** 2

--Clamp a float
clampF :: Float -> Float -> Float -> Float
clampF value mn mx = max (min value mx) mn

--Clamp a point
clampP :: Point -> Point -> Point -> Point
clampP value mn mx = (clampF (fst value) (fst mn) (fst mx),
                     clampF (snd value) (snd mn) (snd mx))

--Length of a point (vector)
lengthP :: Point -> Float
lengthP (x, y) = sqrt (x * x + y * y)

--Quadruple
fst4 (a, _, _, _) = a
snd4 (_, a, _, _) = a
thd4 (_, _, a, _) = a
fth4 (_, _, _, a) = a