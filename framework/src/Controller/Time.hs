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
									 -- enemies = updEnemies,
									  bullets = updBullets,
                                      cameraPos = snd updPlayer
                                      }
                                      where
                                      updPlayer = updatePlayer time player world
                                      --updEnemies = map (updateEnemies time world) enemies 
                                      updBullets = updateBullets time bullets

--Update the player ship
updatePlayer :: Float -> Ship -> World -> (Ship, Point)
updatePlayer time player@(Ship {..}) (World {movementAction, rotateAction, worldWidth, worldHeight})
    = (player {
    sPos = newPos,
    sRot = rotate rotateAction 4,
    sVelocity = newVelocity,
    sForce = calcThrust movementAction sRot 2000
    }, newPos)
    where
    newVelocity = calcVelocity time player
    --Calculate the position
    newPos = clampP updatePosition (0, 0) (worldWidth, worldHeight)
    updatePosition = sPos + newVelocity .* time
    --Rotate the ship
    rotate RotateRight sp = sRot - sp * time
    rotate NoRotation _   = sRot
    rotate RotateLeft sp  = sRot + sp * time

-- | Functions to calculate force, velocity

--Calculate the force and direction
calcThrust :: MovementAction -> Float -> Float -> Point
calcThrust Thrust rot pow = (cos rot, sin rot) .* pow
calcThrust NoMovement _ _ = (0, 0)

--Calculate the velocity
calcVelocity :: Float -> Ship -> Point
calcVelocity time (Ship {..}) = (sVelocity + sForce .* (1 / sMass)) ./ (1 + sFriction * time)

--updateEnemies :: Float -> Ship -> World -> Ship
--updateEnemies = id
--update method for the fired bullets
updateBullets time bullets@(x:xs) = map (\Bullet{..} -> bTimer <= 0 if x updFired) bullets >>> 
                   where
                   updFired bullet@(Bullet{..}) = bullet {bPos = bPos .+. bVelocity, bTimer = if bTimer <= 0 then  bTimer - time}

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
