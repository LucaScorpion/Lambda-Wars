module PointOperators where

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