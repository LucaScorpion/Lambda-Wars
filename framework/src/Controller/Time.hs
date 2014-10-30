{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List
import Data.Maybe

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
                                      rndGen = snd exhParticles,
                                      player = snd updShCollisions,
                                      cameraPos = snd updPlayer,
                                      enemies = fst updShCollisions,
                                      bullets = snd updBulCollisions,
                                      nextSpawn = if nextSpawn <= 0 then spawnTime else nextSpawn - time,
                                      particles = fst exhParticles,
                                      score = if isJust explPos then score + 1 else score
                                      }
                                      where
                                      --Updated enemy and bullet list
                                      updBulCollisions = checkBulCollisions updEnemies updBullets
                                      updShCollisions = checkShCollisions (fst $ fst updBulCollisions) (fst updPlayer) time
                                      updEnemies = map (updateEnemy time world (fst updPlayer)) (spawnEnemy newEnemy enemies)
                                      updBullets = updateBullets shootAction time (delOldBullets bullets) (createBullet player)
                                      --Enemy spawning
                                      spawnPos = randomP (-1000,1000) (-1000,1000) rndGen
                                      newEnemy = if nextSpawn <= 0 then Just (createEnemy (fst spawnPos) (enemySpr !! 0)) else Nothing
                                      updPlayer = updatePlayer time player world
									  -- Particle updating
                                      explPos = snd $ fst updBulCollisions
                                      exhParticles = exhaustParticles (snd spawnPos) movementAction player (updateParticles particles time 10)
									  

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
checkBulCollisions :: [Ship] -> [Bullet] -> (([Ship], Maybe Point), [Bullet])
checkBulCollisions enemies bullets = (collideEnemy enemies, collideBullet bullets)
                                where
                                collideEnemy [] = ([], Nothing)
                                collideEnemy (x@(Ship {..}):xs) | bullCol x = (hitEnemy x ++ xs, Just sPos)
                                                                | otherwise = (x : fst collRest, snd collRest)
                                                                where
                                                                collRest = collideEnemy xs    
                                bullCol enemy = or (map (checkBulletCollision enemy) bullets)
                                collideBullet = filter (\ b -> not $ shipCol b)
                                shipCol bullet = or (map (\ e -> checkBulletCollision e bullet) enemies)
                                hitEnemy enemy@(Ship{..}) = if sLifes > 1 then [enemy{sLifes = sLifes - 1, sInvuln = 1}] else []
                                checkBulletCollision (Ship {sPos, sSize}) (Bullet {bPos}) = abs (sPos .<>. bPos) <= sSize

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
                      sMass = 40,
                      sFriction = 1.5,
                      sRotSpeed = 4,
                      sPower = 250,
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
updateParticles :: [Particle] -> Float -> Float -> [Particle]
updateParticles [] _ _ = []
updateParticles (x@(Particle {..}):xs) time sizeLerp = if pTimer > 0
                                                       then updateParticle time sizeLerp x : updateParticles xs time sizeLerp
                                                       else updateParticles xs time sizeLerp


updateParticle :: Float -> Float -> Particle -> Particle
updateParticle time sizeLerp particle@(Particle {..}) = particle {
                                                        pTimer = pTimer - time,
                                                        pPos = pPos + pVelocity,
                                                        pSize = pSize - sizeLerp * time
                                                        }

--Ship exhaust particles
exhaustParticles :: StdGen -> MovementAction -> Ship -> [Particle] -> ([Particle], StdGen)
exhaustParticles rndGen NoMovement _ particles = (particles,rndGen)
exhaustParticles rndGen Thrust ship particles  = ((exhaustParticle ship (fst offset) (fst rndLife)) : particles, (snd rndLife))
                                               where
                                               offset = randomR (-1.5 ,1.5) rndGen
                                               rndLife = randomR (0.5, 1.0) (snd offset)

exhaustParticle :: Ship -> Float -> Float -> Particle
exhaustParticle (Ship {sPos, sRot}) offset rndLife = Particle {
                                                     pPos = sPos .+. ((-cos sRot, -sin sRot) .* 32),
                                                     pVelocity = (-cos sRot, -sin sRot) + ((sin sRot, -cos sRot) .* offset) ,
                                                     pColor = makeColor 0.7 0.7 0.7 0.2,
                                                     pTimer = rndLife,
                                                     pSize = 10
                                                     }

--Explosion particles
explosionParticle :: Point -> [Particle]
explosionParticle pos = replicate 100 newParticle
                      where
                      newParticle = Particle {
                                    pPos = pos,
                                    pVelocity = (0, 0),
                                    pColor = makeColor 1.0 0.5 0.0 1.0,
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
