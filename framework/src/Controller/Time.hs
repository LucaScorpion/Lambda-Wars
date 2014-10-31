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
timeHandler time world@(World {..}) = if checkplayerlife player
                                      then updateWorld time world
                                      else initial (fst $ random rndGen) (playerSpr player : enemySpr)
                                    where 
                                    checkplayerlife (Ship{..}) = sLifes > 0
                                    playerSpr (Ship{..}) = sSprite

updateWorld time world@(World {..}) = world {
                                      rndGen = snd collExpParticles,
                                      player = snd updShCollisions,
                                      cameraPos = snd updPlayer,
                                      enemies = fst $ fst updShCollisions,
                                      bullets = snd updBulCollisions,
                                      nextSpawn = if nextSpawn <= 0 then spawnTime else nextSpawn - time,
                                      exhaustP = updExhParticles,
                                      explosionP = updExpParticles,
                                      score = if isJust expPos then score + 1 else score
                                      }
                                      where
                                      --Updated enemy and bullet list
                                      updBulCollisions = checkBulCollisions updEnemies updBullets
                                      updShCollisions = checkShCollisions (fst $ fst updBulCollisions) (fst updPlayer) time
                                      updEnemies = map (updateEnemy time world $ fst updPlayer) (spawnEnemy newEnemy enemies)
                                      updBullets = updateBullets shootAction time (delOldBullets bullets) (createBullet player)
                                      --Enemy spawning
                                      spawnPos = randomP (-1000,1000) (-1000,1000) rndGen
                                      newEnemy = if nextSpawn <= 0 then Just (createEnemy (fst spawnPos) (enemySpr !! 0)) else Nothing
                                      updPlayer = updatePlayer time player world
									  -- Particle updating
                                      updExhParticles = fst exhParticles1 ++ fst exhParticles2 ++ updateParticles exhaustP time (-20) 0.5
                                      exhParticles1 = exhaustPlayParticles (snd spawnPos) movementAction player
                                      exhParticles2 = exhaustEnemyParticles (snd exhParticles1) enemies
                                      updExpParticles = fst collExpParticles ++ fst expParticles ++ updateParticles explosionP time 10 1.0
                                      expParticles = if isJust expPos then explosionParticles (snd exhParticles2) 200 3 (fromJust expPos) else ([],snd exhParticles2)
                                      expPos = snd $ fst updBulCollisions
                                      collExpParticles = if isJust playerHit then explosionParticles (snd expParticles) 1000 10 (fromJust playerHit) else ([], snd expParticles)
                                      playerHit = snd $ fst updShCollisions

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
checkShCollisions :: [Ship] -> Ship -> Float -> (([Ship], Maybe Point), Ship)
checkShCollisions enemies player@(Ship {sPos}) time = (filterNoPCol enemies, updECol player)
                                                    where
                                                    --Check if enemy collides with player
                                                    filterNoPCol [] = ([], Nothing)
                                                    filterNoPCol (x:xs) | playCol x = (xs, Just sPos)
                                                                        | otherwise = (x : fst collRest, snd collRest)
                                                                        where
                                                                        collRest = filterNoPCol xs
                                                    playCol ship = checkShipCollision ship player
                                                    --Check if player collides with enemy
                                                    updECol p@(Ship {..}) = if enemyCol p && sInvuln <= 0
                                                                            then hitPlayer p
                                                                            else p { sInvuln = sInvuln - time }
                                                    enemyCol p = or (map (checkShipCollision p) enemies)
                                                    hitPlayer p@(Ship {..}) = p { sLifes = sLifes - 1, sInvuln = 2 }

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
                                                  dist = lengthP (dX, dY)
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
updateParticles :: [Particle] -> Float -> Float -> Float -> [Particle]
updateParticles [] _ _ _ = []
updateParticles (x@(Particle {..}):xs) time sL aL = if pTimer > 0
                                                    then updateParticle time sL aL x : updateParticles xs time sL aL
                                                    else updateParticles xs time sL aL


updateParticle :: Float -> Float -> Float -> Particle -> Particle
updateParticle time sizeLerp alphaLerp particle@(Particle {..})
    = particle {
      pTimer = pTimer - time,
      pPos = pPos + pVelocity,
      pSize = pSize - sizeLerp * time,
      pColor = makeColor (fst4 oldC) (snd4 oldC) (thd4 oldC) ((fth4 oldC) - alphaLerp * time)
      }
      where
      oldC = rgbaOfColor pColor

--Ship exhaust particles
exhaustPlayParticles :: RandomGen g => g -> MovementAction -> Ship -> ([Particle], g)
exhaustPlayParticles rndGen NoMovement _= ([],rndGen)
exhaustPlayParticles rndGen Thrust ship  = ([exhaustParticle ship (fst offset) (fst rndLife)], (snd rndLife))
                                               where
                                               offset = randomR (-1.5 ,1.5) rndGen
                                               rndLife = randomR (0.4, 0.8) (snd offset)

exhaustParticle :: Ship -> Float -> Float -> Particle
exhaustParticle (Ship {sPos, sRot}) offset rndLife = Particle {
                                                     pPos = sPos .+. ((-cos sRot, -sin sRot) .* 32),
                                                     pVelocity = (-cos sRot, -sin sRot) .* 2 + ((sin sRot, -cos sRot) .* offset) ,
                                                     pColor = makeColor 0.7 0.7 0.7 0.2,
                                                     pTimer = rndLife,
                                                     pSize = 7
                                                     }

exhaustEnemyParticles :: RandomGen g => g -> [Ship] -> ([Particle], g)
exhaustEnemyParticles rndGen  []    = ([],rndGen)
exhaustEnemyParticles rndGen (x:xs) = (newParticle x : (fst otherexhpar), snd otherexhpar)
                                      where
                                      otherexhpar = exhaustEnemyParticles (snd rndLife) xs
                                      newParticle (Ship{..})  = Particle {
                                                                pPos = sPos .+. ((-cos sRot, -sin sRot) .* 25),
                                                                pVelocity = (-cos sRot, -sin sRot) .* 2 + ((sin sRot, -cos sRot) .* (fst offset)) ,
                                                                pColor = makeColor 0.7 0.7 0.7 0.2,
                                                                pTimer = fst rndLife,
                                                                pSize = 7
                                                                }
                                      offset  = randomR (-1.5 ,1.5) rndGen
                                      rndLife = randomR (0.4, 0.8) (snd offset)

--Explosion particles rndGen, Float, [Particle], Point
explosionParticles :: RandomGen g => g -> Float -> Float -> Point -> ([Particle],g)
explosionParticles rndGen 0 _ _         = ([],rndGen)
explosionParticles rndGen amount sp pos = (newParticle : (fst otherexppar), snd otherexppar)
                                              where
                                              otherexppar = explosionParticles (snd rndA) (amount - 1) sp pos
                                              newParticle = Particle {
                                                            pPos = pos,
                                                            pVelocity = (fst rndVel) ./ (max 1.0 (lengthP $ fst rndVel)) .* sp,
                                                            pColor = makeColor (fst rndR) (fst rndG) 0.0 (fst rndA),
                                                            pTimer = fst rndLife,
                                                            pSize = 10
                                                            }
                                              rndVel  = randomP (-1, 1) (-1,1) rndGen
                                              rndLife = randomR (0.3, 1.0 :: Float) (snd rndVel)
                                              rndR    = randomR (0.6, 1.0 :: Float)  (snd rndLife)
                                              rndG    = randomR (0.0, 0.3 :: Float) (snd rndR)
                                              rndA    = randomR (0.4, 0.7 :: Float) (snd rndG)
															
-- | Helper functions

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