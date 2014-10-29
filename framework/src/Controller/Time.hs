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
									  bullets = updBullets,
                                      cameraPos = snd updPlayer,
                                      nextSpawn = if spawnEnemy then spawnTime else nextSpawn - time,
                                      enemies = updEnemies
                                      }
                                      where
                                      spawnEnemy = nextSpawn <= 0
                                      newEnemy = if spawnEnemy then Just (createEnemy spawnPos enemySpr) else Nothing
                                      spawnPos = (0, 0)
                                      updPlayer = updatePlayer time player world
                                      updEnemies = map (updateEnemies time world) (updateEnemyList newEnemy enemies)
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

-- | Enemy updating

--Create an enemy
createEnemy :: Point -> Picture -> Ship
createEnemy pos spr = Ship {
sSprite = spr,
sPos = pos,
sRot = degToRad 90,
sForce = (0,0),
sVelocity = (0,0),
sMass = 50,
sFriction = 3,
sRotSpeed = 4,
sPower = 500,
sAlive = True
}

--Update the list of enemies (remove dead ships)
updateEnemyList mShip [] = case mShip of
                           Nothing -> []
                           Just ship -> [ship]
updateEnemyList mShip (x@(Ship{sAlive}):xs) = case mShip of
                                              Nothing -> newList
                                              Just ship -> ship : newList
                                            where
                                            newList = if (sAlive)
                                                      then x : updateEnemyList Nothing xs
                                                      else updateEnemyList Nothing xs

updateEnemies :: Float -> World -> Ship -> Ship
updateEnemies time (World {..}) enemy@(Ship {..}) = enemy {
                                                    sAlive = True--not $ checkCollision player enemy
                                                    }

--Check for collision between 2 ships
checkCollision :: Ship -> Ship -> Bool
checkCollision (Ship {sPos = pos1}) (Ship {sPos = pos2}) = False

-- | Bullet updating

delOldBullets [] = []
delOldBullets (y@(Bullet{..}):ys) = if bTimer > 0
                                    then y : delOldBullets ys
                                    else delOldBullets ys

--Update method for the fired bullets
updateBullets Shoot time bullets (Ship {..}) = newBullet : (map (updFired time) bullets)
                                             where
                                             newBullet = Bullet {
                                             bPos = sPos,
                                             bVelocity = (cos sRot, sin sRot) .* 20,
                                             bTimer = 0.5
                                             }
updateBullets DontShoot time bullets _ = map (updFired time) bullets

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
