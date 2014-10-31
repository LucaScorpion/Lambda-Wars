{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Geometry

-- | Game state

data World = World {
        --Random generator
        rndGen           :: StdGen,
        --World size
        worldWidth       :: Float,
        worldHeight      :: Float,
        --Actions
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
		--Background
        stars            :: [Star],
		--Player and enemies
        player           :: Ship,
        enemies          :: [Ship],
        enemySpr         :: [Picture],
        --Enemy spawning
        spawnTime        :: Float,
        nextSpawn        :: Float,
        --Shooting
        bullets          :: [Bullet],
		--Camera
        cameraPos        :: Point,
        --Score
        items            :: [Item],
        iPic             :: [Picture],
        itemTime         :: Float,
        nextItem         :: Float,
        --Particles
        exhaustP         :: [Particle],
        explosionP       :: [Particle]
        }

--Actions
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving Eq
data MovementAction = NoMovement | Thrust
    deriving Eq
data ShootAction    = Shoot      | DontShoot
    deriving Eq

--Star position and depth
type Star = (Point, Float)

data Ship = Ship {
    --Ship sprite
    sSprite     :: Picture,
    --Position and rotation
    sPos        :: Point,
    sRot        :: Float,
    --Physics properties
    sForce      :: Point,
    sVelocity   :: Point,
    sMass       :: Float,
    sFriction   :: Float,
    sRotSpeed   :: Float,
    sPower      :: Float,
    sSize       :: Float,
    --Alive or dead
    sLifes      :: Float,
	sInvuln     :: Float,
    sScore      :: Int,
    sMultiply   :: Int,
    --Reloading
    sReloadTime :: Float,
    sReloading  :: Float,
    sItemtime   :: Float,
    sEffects    :: [Item]
    }

data Bullet = Bullet {
    bPos      :: Point,
    bVelocity :: Point,
    bTimer    :: Float
    }
		   
data Item =   Multiplier {iPos :: Point, iPicture :: Picture, iTimer :: Float}
             | Invulnerable {iPos :: Point, iPicture :: Picture, iTimer :: Float}
            
	
data Particle = Particle {
    pPos      :: Point,
    pVelocity :: Point,
    pColor    :: Color,
    pTimer    :: Float,
    pSize     :: Float
    }

--Generate the initial world
initial :: Int -> [Picture] -> World
initial seed (pl:en) = generateStars newWorld
                     where
                     newWorld = World {
                              rndGen = mkStdGen seed,
                              worldWidth     = 2000,
                              worldHeight    = 2000,
                              rotateAction   = NoRotation,
                              movementAction = NoMovement,
                              shootAction    = DontShoot,
                              stars          = [],
                              player         = createPlayer pl,
                              enemySpr       = en,
                              enemies        = [],
                              spawnTime      = 3,
                              nextSpawn      = 3,
                              bullets        = [],
                              cameraPos      = (0, 0),
							  items          = [],
                              iPic           = createIPic,
                              itemTime       = 15,
                              nextItem       = 10,
                              exhaustP       = [],
                              explosionP     = []
                              }

--Create a player ship
createPlayer :: Picture -> Ship
createPlayer plSpr = Ship {
    sSprite     = plSpr,
    sPos        = (0,0),
    sRot        = degToRad 90,
    sForce      = (0,0),
    sVelocity   = (0,0),
    sMass       = 40,
    sFriction   = 1.5,
    sRotSpeed   = 3.5,
    sPower      = 500,
    sSize       = 26,
    sLifes      = 3,
    sInvuln     = 0,
    sScore      = 0,
    sMultiply   = 1,
    sReloading  = 0,
    sReloadTime = 0.1,
    sItemtime   = 0,
    sEffects    = []
    }

createIPic :: [Picture]
createIPic = [scale 0.3 0.3 (color yellow (text "p")), scale 0.3 0.3 (color blue (text "i"))]
	
--Generate the stars
generateStars :: World -> World
generateStars world@(World {rndGen, stars}) = world { rndGen = fst rnds, stars = newStars amount }
                                            where 
											--Create a random amount of stars
                                            amount = randomR (400 :: Int, 700 :: Int) (snd rnds)
											--Create 2 new random generators
                                            rnds = split rndGen
											--List of stars
                                            newStars (0,g) = []
                                            newStars (a,g) = (fst p, fst f) : newStars (a-1,snd f)
                                                           where
                                                           p = randomP (-1.1, 1.1) (-1.1, 1.1) g
                                                           f = randomR (0.0, 1.0) (snd p)

--Generate a random point
randomP :: RandomGen g => Point -> Point -> g -> (Point, g)
randomP p1 p2 g = let (w, g') = randomR (fst p1, snd p1) g
                  in let (h, g'') = randomR (fst p2, snd p2) g'
                     in ((w, h), g'')