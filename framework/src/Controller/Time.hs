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
                                      rndGen = snd spawnPos,
                                      player = fst updPlayer,
                                      cameraPos = snd updPlayer,
                                      enemies = fst updCollisions,
                                      bullets = snd updCollisions,
                                      nextSpawn = if nextSpawn <= 0 then spawnTime else nextSpawn - time
                                      }
                                      where
                                      --Updated enemy and bullet list
                                      updCollisions = checkCollisions updEnemies updBullets
                                      updEnemies = map (updateEnemy time world (fst updPlayer)) (spawnEnemy newEnemy enemies)
                                      updBullets = updateBullets shootAction time (delOldBullets bullets) (createBullet player)
                                      --Enemy spawning
                                      spawnPos = randomP (-1000,1000) (-1000,1000) rndGen
                                      newEnemy = if nextSpawn <= 0 then Just (createEnemy (fst spawnPos) (enemySpr !! 0)) else Nothing
                                      updPlayer = updatePlayer time player world

--Update the player ship
updatePlayer :: Float -> Ship -> World -> (Ship, Point)
updatePlayer time pl@(Ship {..}) (World {..})
    = (pl {
    sPos = newPos,
    sRot = normaliseAngle (rotateShip rotateAction time player),
    sVelocity = newVelocity,
    sForce = calcThrust movementAction player,
    sReloading = rldTime
    }, newPos)
    where
    rldTime = if shootAction == Shoot && sReloading <= 0
              then sReloadTime
              else max 0 (sReloading - time)
    newVelocity = calcVelocity time player
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

-- | Collisions
checkCollisions :: [Ship] -> [Bullet] -> ([Ship], [Bullet])
checkCollisions enemies bullets = ([e | e <- filterNoBCol enemies], [b | b <- filterNoSCol bullets])
                                where
                                filterNoBCol = filter (\ s -> not $ bullCol s)
                                bullCol ship = or (map (checkBulletCollision ship) bullets)
                                filterNoSCol = filter (\ b -> not $ shipCol b)
                                shipCol bullet = or (map (\ e -> checkBulletCollision e bullet) enemies)

--Check for collision between 2 ships
checkShipCollision :: Ship -> Ship -> Bool
checkShipCollision (Ship {sPos = pos1}) (Ship {sPos = pos2}) = abs (pos1 .<>. pos2) <= 64

--Check for collision between a ship and a bullet
checkBulletCollision :: Ship -> Bullet -> Bool
checkBulletCollision (Ship {sPos}) (Bullet {bPos}) = abs (sPos .<>. bPos) <= 32

-- | Enemy updater

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

--Spawn an enemy
spawnEnemy mShip enemies = case mShip of
                           Nothing -> enemies
                           Just ship -> ship : enemies

--Update an enemy ship
updateEnemy :: Float -> World -> Ship -> Ship -> Ship
updateEnemy time (World {..}) playert@(Ship {sPos = pPos}) enemy@(Ship {..}) = enemy {
                                                  sPos = sPos,
                                                  sRot = anglepe, -- normaliseAngle $ rotateShip rotR time enemy,
                                                  sVelocity = newVelocity,
                                                  sForce = calcThrust Thrust enemy
                                                  }
                                                  where
                                                  rotR = (if anglepe >= pi || (anglepe < 0 && anglepe > -pi )  then RotateRight else rotL)
                                                  rotL = (if anglepe <= -pi || (anglepe > 0 && anglepe < pi )  then RotateRight else NoRotation)
                                                  anglepe = (acos ((fst anglep)/(sPos.<>.pPos)))
                                                  anglep = sPos .-. pPos
                                                  newVelocity = calcVelocity time enemy
                                                  newPos = clampP updatePosition (-worldWidth / 2, -worldHeight / 2) (worldWidth / 2, worldHeight / 2)
                                                  updatePosition = sPos + newVelocity .* time

-- | Bullet updater

--Create a bullet if the ship can shoot, else nothing
createBullet :: Ship -> Maybe Bullet
createBullet (Ship {sPos, sRot, sReloading}) = if sReloading <= 0 then Just newBullet else Nothing
                                             where
                                             newBullet = Bullet {
                                             bPos = sPos,
                                             bVelocity = (cos sRot, sin sRot) .* 20,
                                             bTimer = 0.5
                                             }

--Update method for the fired bullets
updateBullets :: ShootAction -> Float -> [Bullet] -> Maybe Bullet -> [Bullet]
updateBullets Shoot time bullets mBul  = case mBul of
                                         Nothing -> map (updFired time) bullets
                                         Just bullet -> bullet : (map (updFired time) bullets)
updateBullets DontShoot time bullets _ = map (updFired time) bullets

--Remove old bullets
delOldBullets :: [Bullet] -> [Bullet]
delOldBullets [] = []
delOldBullets (y@(Bullet{..}):ys) = if bTimer > 0
                                    then y : delOldBullets ys
                                    else delOldBullets ys

--Update a bullet
updFired :: Float -> Bullet -> Bullet
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
