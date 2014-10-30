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
timeHandler time world@(World {..}) = if checkplayerlife player then updateWorld time world else initial (fst(random rndGen)) ((playerSpr player):enemySpr)
                                     where 
                                     checkplayerlife (Ship{..}) = sLifes > 0
                                     playerSpr (Ship{..}) = sSprite

updateWorld time world@(World {..}) = world {
                                      rndGen = snd rnds,
                                      player = snd updShCollisions,
                                      cameraPos = snd updPlayer,
                                      enemies = fst updShCollisions,
                                      bullets = snd updBulCollisions,
                                      nextSpawn = if nextSpawn <= 0 then spawnTime else nextSpawn - time,
                                      particles = updParticles
                                      }
                                      where
                                      rnds = split rndGen
                                      --Updated enemy and bullet list
                                      updBulCollisions = checkBulCollisions updEnemies updBullets
                                      updShCollisions = checkShCollisions (fst updBulCollisions) (fst updPlayer) time
                                      updEnemies = map (updateEnemy time world (fst updPlayer)) (spawnEnemy newEnemy enemies)
                                      updBullets = updateBullets shootAction time (delOldBullets bullets) (createBullet player)
                                      --Enemy spawning
                                      spawnPos = randomP (-1000,1000) (-1000,1000) (fst rnds)
                                      newEnemy = if nextSpawn <= 0 then Just (createEnemy (fst spawnPos) (enemySpr !! 0)) else Nothing
                                      updPlayer = updatePlayer time player world
									  -- Particle updating
                                      updParticles = exhaustParticles movementAction player (updateParticles particles time)
									  

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
checkBulCollisions :: [Ship] -> [Bullet] -> ([Ship], [Bullet])
checkBulCollisions enemies bullets = (collideEnemy enemies, collideBullet bullets)
                                where
                                collideEnemy [] = []
                                collideEnemy (x:xs) = if bullCol x then hitEnemy x ++ xs else x : (collideEnemy xs)
                                bullCol enemy = or (map (checkBulletCollision enemy) bullets)
                                collideBullet = filter (\ b -> not $ shipCol b)
                                shipCol bullet = or (map (\ e -> checkBulletCollision e bullet) enemies)
                                hitEnemy enemy@(Ship{..}) = if sLifes > 1 then [enemy{sLifes = sLifes - 1, sInvuln = 1}] else []
                                checkBulletCollision (Ship {sPos}) (Bullet {bPos}) = abs (sPos .<>. bPos) <= 32

--Check for collision between 2 ships
checkShCollisions :: [Ship] -> Ship -> Float -> ([Ship], Ship)
checkShCollisions enemies player time = ([e | e <- (filterNoPCol enemies player)], updECol player)
                                       where
								       -- check if enemy collides with player
                                       filterNoPCol [] _ = []
                                       filterNoPCol (x:xs) (Ship{..}) = if playCol x then xs else x:(filterNoPCol xs player)
                                       playCol ship = checkShipCollision ship player
						       		   -- check if player collides with enemy
                                       updECol :: Ship -> Ship
                                       updECol playert@(Ship{..}) = if enemyCol playert && sInvuln <= 0 then hitPlayer playert else playert{sInvuln = sInvuln - time}
                                       enemyCol p = or (map (checkShipCollision p) enemies)
                                       hitPlayer playert@(Ship{..}) =playert{sLifes = sLifes - 1, sInvuln = 2}

--Check if 2 ships collide
checkShipCollision :: Ship -> Ship -> Bool
checkShipCollision (Ship {sPos = pos1, sSize = size1}) (Ship {sPos = pos2, sSize = size2})
    = abs (pos1 .<>. pos2) <= size1 + size2

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
                      sSize = 26,
					  sLifes = 1,
                      sInvuln = 0
                      }

--Spawn an enemy
spawnEnemy mShip enemies = case mShip of
                           Nothing -> enemies
                           Just ship -> ship : enemies

--Update an enemy ship
updateEnemy :: Float -> World -> Ship -> Ship -> Ship
updateEnemy time (World {..}) playert@(Ship {sPos = pPos}) enemy@(Ship {..}) = enemy {
                                                  sPos = newPos,
                                                  sRot = (rotateShip rotR time enemy),
                                                  sVelocity = newVelocity,
                                                  sForce = calcThrust Thrust enemy
                                                  }
                                                  where
                                                  rotR = (if rotation >= pi || (rotation < -0.2 && rotation > -pi )  then RotateRight else rotL)
                                                  rotL = (if rotation <= -pi || (rotation > 0.2 && rotation < pi )  then RotateLeft else NoRotation)
                                                  rotation = normaliseAngle ((atan2 (dY / dist) (dX / dist)) - sRot)
                                                  dist = sqrt (dX * dX + dY * dY)
                                                  dY = (snd pPos) - (snd sPos)
                                                  dX = (fst pPos) - (fst sPos)
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

-- | Particles

--Update particles
updateParticles [] _        = []
updateParticles (x:xs) time = if stillthere x then (updateParticle time x) : updateParticles xs time else updateParticles xs time
                              where  
                              stillthere (Particle{pTimer}) = pTimer > 0 


updateParticle :: Float -> Particle -> Particle
updateParticle time particle@(Particle {..}) = particle {
                                               pTimer = pTimer - time,
                                               pPos = pPos + pVelocity
                                               }

--Ship exhaust particles
exhaustParticles :: MovementAction -> Ship -> [Particle] -> [Particle]
exhaustParticles NoMovement _ particles = particles
exhaustParticles Thrust ship particles  = (exhaustParticle ship) : particles

exhaustParticle :: Ship -> Particle
exhaustParticle (Ship {sPos, sRot}) = Particle {
                                      pPos = sPos .+. ((-cos sRot, -sin sRot) .* 32),
                                      pVelocity = (-cos sRot, -sin sRot) ,
                                      pColor = makeColor 0.5 0.5 0.5 0.2,
                                      pTimer = 0.5,
                                      pSize = 10
                                      }

-- | Helper functions

--Clamp a float
clampF :: Float -> Float -> Float -> Float
clampF value mn mx = max (min value mx) mn

--Clamp a point
clampP :: Point -> Point -> Point -> Point
clampP value mn mx = (clampF (fst value) (fst mn) (fst mx),
                     clampF (snd value) (snd mn) (snd mx))
