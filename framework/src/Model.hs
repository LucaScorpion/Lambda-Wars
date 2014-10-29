{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Geometry

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        worldWidth       :: Float,
        worldHeight      :: Float,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
		--Background
        stars            :: [Star],
		--Player and enemies
        player           :: Ship,
        enemies          :: [Ship],
        bullets          :: [Bullet],
		--Camera
        cameraPos        :: Point
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot
--Star position and depth
type Star = (Point, Float)

data Ship = Ship {
sSprite   :: Picture,
sPos      :: Point,
sRot      :: Float,
sForce    :: Point,
sVelocity :: Point,
sMass     :: Float,
sFriction :: Float,
sRotSpeed :: Float,
sPower    :: Float
}

data Bullet = Bullet {
bPos      :: Point,
bVelocity :: Point,
bTimer    :: Float
}

initial :: Int -> Picture -> World
initial seed plSpr = generateStars newWorld
                   where
                   newWorld = World {
						    rndGen = mkStdGen seed,
							worldWidth = 2000,
							worldHeight = 2000,
                            --Actions
						    rotateAction = NoRotation,
						    movementAction = NoMovement,
						    shootAction = DontShoot,
                            --Background
						    stars=[],
                            --Player
							player = createPlayer plSpr,
                            enemies = [],
                            bullets = [],
                            cameraPos = (0, 0)
                            }

--Create a player ship
createPlayer :: Picture -> Ship
createPlayer plSpr = Ship {
sSprite = plSpr,
sPos = (0,0),
sRot = degToRad 270,
sForce = (0,0),
sVelocity = (0,0),
sMass = 50,
sFriction = 3,
sRotSpeed = 4,
sPower = 100
}

							
-- adsfkljaf;kljakljfakljakjafdskjfdsakjfdsakjfdsa';klj
generateStars :: World -> World
generateStars world@(World {rndGen, stars}) = world {rndGen = fst rnds, stars = s}
                                            where 
											--Create a random amount of stars
                                            amount = randomR (400 :: Int, 700 :: Int) (snd rnds)
											--Create 2 new random generators
                                            rnds = split rndGen
											--List of stars
                                            s = newStars amount
                                            newStars (0,g) = []
                                            newStars (a,g) = (fst p, fst f) : newStars (a-1,snd f)
                                                           where
                                                           p = rndPoint g
                                                           f = rndFloat (0.0, 1.0) (snd p)
											--Randoms
                                            rndPoint g = let (w, g') = rndFloat (-1.0, 1.0) g
                                                         in let (h, g'') = rndFloat (-1.0, 1.0) g'
                                                            in ((w, h), g'')
                                            rndFloat :: RandomGen g => (Float, Float) -> g -> (Float, g)
                                            rndFloat r g = randomR r g