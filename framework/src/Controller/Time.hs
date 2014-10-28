{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry

import System.Random

import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) = world {
                                      player = fst updPlayer,
									  enemies = updEnemies,
									  bullets = updBullets,
                                      cameraPos = snd updPlayer
                                      }
                                      where
                                      updPlayer = updatePlayer time player world
                                      updEnemies = map (updateEnemies time world) enemies 
                                      updBullets = map (updateBullets) bullets

updatePlayer :: Float -> Ship -> World -> (Ship, Point)
updatePlayer time player@(Ship {..}) (World {movementAction, rotateAction, worldWidth, worldHeight})
    = (player {
    sPos = newPos,
    sRot = rotate rotateAction 4,
    sVelocity = updateVelocity 0.3 2,
    sForce = calcThrust movementAction 2000
    }, newPos)
	where
    --Calculate the thrust
    calcThrust Thrust pow = (cos sRot, sin sRot) .* pow
    calcThrust NoMovement _ = (0, 0)
    --Calculate the position
    newPos = clampP updatePosition (0, 0) (worldWidth, worldHeight)
    updatePosition = sPos + sVelocity .* time
    --Update velocity and force
    updateVelocity iMass fric = (sVelocity + sForce .* iMass) ./ (1 + fric * time)
    --Direction
    rotate RotateRight sp = sRot - sp * time
    rotate NoRotation _ = sRot
    rotate RotateLeft sp = sRot + sp * time
      
--updateEnemies :: Float -> Ship -> World -> Ship
updateEnemies = id

updateBullets = id

-- | Helper functions

--Clamp a float
clampF :: Float -> Float -> Float -> Float
clampF value mn mx = max (min value mx) mn

--Clamp a point
clampP :: Point -> Point -> Point -> Point
clampP value mn mx = (clampF (fst value) (fst mn) (fst mx),
                     clampF (snd value) (snd mn) (snd mx))

-- | Operators

infixl 7 .*, .*., ./

--Multiply a point and a float
(.*) :: Point -> Float -> Point
(x, y) .* f = (x * f, y * f)

--Multiply two points
(.*.) :: Point -> Point -> Point
(x, y) .*. (z, w) = (x * z, y * w)

--Divide a point by a float
(./) :: Point -> Float -> Point
(x, y) ./ f = (x / f, y / f)