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
									  enemies = updEnemies,
									  bullets = updBullets,
                                      cameraPos = snd updPlayer
                                      }
                                      where
                                      updPlayer = updatePlayer time player world
                                      updEnemies = map (updateEnemies) enemies 
                                      updBullets = map (updateBullets) bullets

updatePlayer :: Float -> Ship -> World -> (Ship, Point)
updatePlayer time player@(Ship {..}) world@(World {movementAction, rotateAction})
    = (player {
    sPos = updatePosition,
    sRot = rotate rotateAction 4,
    sVelocity = updateVelocity 0.3 2,
    sForce = calcThrust movementAction 2000
    }, updatePosition)
	where
    --Calculate the thrust
    calcThrust Thrust pow = (cos sRot, sin sRot) .* pow
    calcThrust NoMovement _ = (0, 0)
    --Calculate the position
    updatePosition = sPos + sVelocity .* time
	--Update velocity and force
    updateVelocity iMass fric = (sVelocity + sForce .* iMass) ./ (1 + fric * time)
	--Direction
    rotate RotateRight sp = sRot - sp * time
    rotate NoRotation _ = sRot
    rotate RotateLeft sp = sRot + sp * time
      
updateEnemies = id

updBullets = id

-- | Helper functions
clampF :: Float -> Float -> Float -> Float
clampF value mn mx = 

-- | Operators

infixl 7 .*, .*., ./

--Multiply a point and a float
(.*) :: Point -> Float -> Point
(x, y) .* f = (x * f, y * f)

--Multiply two points
(.*.) :: Point -> Point -> Point
(x, y) .*. (z, w) = (x * z, y * w)

--Divide a point by a float
(./) :: Point -> Float -> Point
(x, y) ./ f = (x / f, y / f)