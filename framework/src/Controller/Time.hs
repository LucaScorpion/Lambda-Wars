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
                                      updBullets = updateBullets shootAction time (delOldBullets bullets) (fst updPlayer)

--Update the player ship
updatePlayer :: Float -> Ship -> World -> (Ship, Point)
updatePlayer time player@(Ship {..}) (World {movementAction, rotateAction, worldWidth, worldHeight})
    = (player {
    sPos = newPos,
    sRot = rotateShip rotateAction time player,
    sVelocity = newVelocity,
    sForce = calcThrust movementAction player
    }, newPos)
    where
    newVelocity = calcVelocity time player
    --Calculate the position
    newPos = clampP updatePosition (0, 0) (worldWidth, worldHeight)
    updatePosition = sPos + newVelocity .* time

-- | Functions to calculate force, velocity

--Calculate the force and direction
calcThrust :: MovementAction -> Ship -> Point
calcThrust Thrust (Ship {sRot, sPower}) = (cos sRot, sin sRot) .* sPower
calcThrust NoMovement _                 = (0, 0)

--Calculate the velocity
calcVelocity :: Float -> Ship -> Point
calcVelocity time (Ship {..}) = (sVelocity + sForce .* (1 / sMass)) ./ (1 + sFriction * time)

--Rotate a ship
rotateShip :: RotateAction -> Float -> Ship -> Float
rotateShip RotateRight time (Ship {sRot, sRotSpeed}) = sRot - sRotSpeed * time
rotateShip NoRotation _ (Ship {sRot})                = sRot
rotateShip RotateLeft time (Ship {sRot, sRotSpeed})  = sRot + sRotSpeed * time

-- | Bullet updating

--updateEnemies :: Float -> Ship -> World -> Ship
--updateEnemies = id

--Update method for the fired bullets
updateBullets Shoot time bullets (Ship {..}) = (map (updFired time) bullets)
                                             where
                                             newBullet = Bullet {
                                             bPos = sPos,
                                             bVelocity = (cos sRot, sin sRot) .* 20,
                                             bTimer = 5
                                             }
updateBullet DontShoot time bullets (Ship {..}) = map (updFired time) bullets

--Remove old bullets
delOldBullets [] = []
delOldBullets (y@(Bullet{..}):ys) = if bTimer > 0 then y : delOldBullets ys else delOldBullets ys

--Update a bullet
updFired time bullet@(Bullet{..}) = bullet {
                                    bPos = bPos .+. bVelocity,
                                    bTimer = bTimer - time
                                    }

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
