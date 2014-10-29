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
import PointOperators

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) = world {
                                      player = fst updPlayer,
                                      -- enemies = updEnemies,
                                      spawnTimer = spawnTimer - time,
                                      bullets = fst updBullets,
                                      reload = snd updBullets,
                                      cameraPos = snd updPlayer
                                      }
                                      where
                                      updPlayer = updatePlayer time player world
                                      --updEnemies = map (updateEnemies time world) enemies 
                                      updBullets = updateBullets shootAction time reload (delOldBullets bullets) (fst updPlayer)

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
    newPos = clampP updatePosition (-worldWidth / 2, -worldHeight / 2) (worldWidth / 2, worldHeight / 2)
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
updateBullets Shoot time reload bullets (Ship {..}) = if reload > 0 then (map (updFired time) bullets, reload - time) else (newBullet : (map (updFired time) bullets), reloadTime)
                                             where
                                             newBullet = Bullet {
                                             bPos = sPos,
                                             bVelocity = (cos sRot, sin sRot) .* 20,
                                             bTimer = 5
                                             }
											 reloadTime = 0.5
updateBullets DontShoot time reload bullets _ = (map (updFired time) bullets, reload - time)

--Remove old bullets
delOldBullets [] = []
delOldBullets (y@(Bullet{..}):ys) = if bTimer > 0
                                    then y : delOldBullets ys
                                    else delOldBullets ys

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
