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
import Functions
import Particles
import Items

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) = if alive player || invuln player
                                      then updateWorld time world
                                      else initial (fst $ random rndGen) (playerSpr player : enemySpr)
                                    where 
                                    alive (Ship {sLifes}) = sLifes > 0
                                    invuln (Ship {sInvuln}) = sInvuln > 0
                                    playerSpr (Ship {sSprite}) = sSprite

updateWorld time world@(World {..}) = world {
                                      rndGen     = snd rndItem,
                                      player     = fst updItemCollisions,
                                      cameraPos  = snd updPlayer,
                                      enemies    = if isJust playerHit then [] else enemyPoss,
                                      bullets    = snd updBulCollisions,
                                      nextSpawn  = if nextSpawn <= 0 then spawnTime else nextSpawn - time,
									  items      = snd updItemCollisions,
                                      nextItem   = if nextItem <= 0 then itemTime else nextItem - time,
                                      exhaustP   = updExhParticles,
                                      explosionP = updExpParticles
                                      }
                                      where
                                      --Updated enemy and bullet list
                                      updBulCollisions = checkBulCollisions updEnemies updBullets
                                      expPos = snd $ fst updBulCollisions
                                      updShCollisions = checkShCollisions (fst $ fst updBulCollisions) (fst updPlayer) time
                                      playerHit = snd $ fst updShCollisions
                                      enemyPoss = fst $ fst updShCollisions
                                      updEnemies = map (updateEnemy time world $ player) newEnemy
                                      updBullets = updateBullets shooting time (delOldBullets bullets) (createBullet player)
                                      shooting = if shipAlive player then shootAction else DontShoot
                                      --Enemy spawning
                                      spawnPos = randomP (-1000,1000) (-1000,1000) rndGen
                                      newEnemy = if nextSpawn <= 0 then (createEnemy (fst spawnPos) (enemySpr !! 0) : enemies)  else enemies
                                      updPlayer = updatePlayer time player world (isJust expPos)
									  -- Particle updating
                                      updExhParticles = fst exhParticles1 ++ fst exhParticles2 ++ updateParticles exhaustP time (-20) 0.5
                                      exhParticles1 = exhaustPlayParticles (snd spawnPos) moving player
                                      moving = if shipAlive player then movementAction else NoMovement
                                      exhParticles2 = exhaustEnemyParticles (snd exhParticles1) enemies
                                      updExpParticles = fst enemyExpParticles ++ fst playerExpParticles ++ fst expParticles ++ updateParticles explosionP time 10 1.0
                                      expParticles = if isJust expPos then explosionParticles (snd exhParticles2) 200 3 $ fromJust expPos else ([],snd exhParticles2)
                                      playerExpParticles = if isJust playerHit then explosionParticles (snd expParticles) 1000 10 $ fromJust playerHit else ([], snd expParticles)
                                      enemyExpParticles = if isJust playerHit then mulExplParticles (snd playerExpParticles) 200 3 $ map shipPos enemyPoss else ([], snd playerExpParticles)
									  -- Item updating
                                      updItemCollisions = checkICollision (snd updShCollisions) updItems
                                      updItems = updateItems time newItem
                                      itemPos = randomP (-1000,1000) (-1000,1000) (snd enemyExpParticles)
                                      newItem = if nextItem <= 0 then createItem (fst itemPos) (fst rndItem) iPic : items else items
                                      rndItem = randomR (0, 1) (snd itemPos)

shipPos :: Ship -> Point
shipPos (Ship {sPos}) = sPos

shipAlive :: Ship -> Bool
shipAlive (Ship {sLifes}) = sLifes > 0

--Update the player ship
updatePlayer :: Float -> Ship -> World -> Bool -> (Ship, Point)
updatePlayer time pl@(Ship {..}) (World {..}) plusscore
    = (pl {
    sPos = newPos,
    sRot = normaliseAngle (rotateShip rotateAction time player),
    sVelocity = newVelocity,
    sForce = if sLifes > 0 then calcThrust movementAction player else (0, 0),
    sReloading = rldTime,
    sScore = if plusscore then sScore + sMultiply else sScore
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
checkBulCollisions enemies bullets = (collideEnemy enemies, collideBullet)
                                where
                                collideEnemy [] = ([], Nothing)
                                collideEnemy (x@(Ship {..}):xs) | bullCol x = (hitEnemy x ++ xs, Just sPos)
                                                                | otherwise = (x : fst collRest, snd collRest)
                                                                where
                                                                collRest = collideEnemy xs    
                                bullCol enemy = or (map (checkBulletCollision enemy) bullets)
                                collideBullet = filter (\b -> not $ shipCol b) bullets
                                shipCol bullet = or (map (\ e -> checkBulletCollision e bullet) enemies)
                                hitEnemy enemy@(Ship{..}) = if sLifes > 1 then [enemy {sLifes = sLifes - 1, sInvuln = 1}] else []
                                checkBulletCollision (Ship {sPos, sSize}) (Bullet{bPos}) = abs (sPos .<>. bPos) <= sSize


checkICollision :: Ship -> [Item] -> (Ship,[Item])
checkICollision player items = (collidePlayer, collideItem items)
                                where
                                collidePlayer = case itemCol items of
                                                Nothing -> player
                                                Just x -> applyItem player x
                                itemCol []     = Nothing
                                itemCol (x:xs) = if checkItemCollision player x then Just x else itemCol xs
                                collideItem [] = []
                                collideItem (x:xs) = if checkItemCollision player x then xs else x : collideItem xs
                                checkItemCollision (Ship {sPos, sSize}) item = abs (sPos .<>. (getItempos item)) <= sSize
								
								
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
					  sLifes = 1
                      }

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
                                         Just bullet -> bullet : map (updFired time) bullets
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