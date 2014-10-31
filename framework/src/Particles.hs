{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}

module Particles where

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry

import System.Random

import Model
import Functions

--Update a list of particles
updateParticles :: [Particle] -> Float -> Float -> Float -> [Particle]
updateParticles [] _ _ _ = []
updateParticles (x@(Particle {..}):xs) time sL aL = if pTimer > 0
                                                    then updateParticle x time sL aL : updateParticles xs time sL aL
                                                    else updateParticles xs time sL aL

--Update a particle
updateParticle :: Particle -> Float -> Float -> Float -> Particle
updateParticle particle@(Particle {..}) time sizeLerp alphaLerp
    = particle {
      pTimer = pTimer - time,
      pPos = pPos + pVelocity,
      pSize = pSize - sizeLerp * time,
      pColor = makeColor (fst4 oldC) (snd4 oldC) (thd4 oldC) ((fth4 oldC) - alphaLerp * time)
      }
      where
      oldC = rgbaOfColor pColor

--Player exhaust particles
exhaustPlayParticles :: RandomGen g => g -> MovementAction -> Ship -> ([Particle], g)
exhaustPlayParticles rndGen NoMovement _= ([],rndGen)
exhaustPlayParticles rndGen Thrust ship  = ([exhaustParticle ship (fst offset) (fst rndLife)], (snd rndLife))
                                               where
                                               offset = randomR (-1.5 ,1.5) rndGen
                                               rndLife = randomR (0.4, 0.8) (snd offset)

--Create an exhaust particle
exhaustParticle :: Ship -> Float -> Float -> Particle
exhaustParticle (Ship {sPos, sRot}) offset rndLife = Particle {
                                                     pPos = sPos .+. ((-cos sRot, -sin sRot) .* 32),
                                                     pVelocity = (-cos sRot, -sin sRot) .* 2 + ((sin sRot, -cos sRot) .* offset) ,
                                                     pColor = makeColor 0.7 0.7 0.7 0.2,
                                                     pTimer = rndLife,
                                                     pSize = 7
                                                     }

--Enemy exhaust particles
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

--Create particles for one explosion
explosionParticles :: RandomGen g => g -> Float -> Float -> Point -> ([Particle], g)
explosionParticles rndGen 0 _ _         = ([],rndGen)
explosionParticles rndGen amount sp pos = (newParticle : fst otherexppar, snd otherexppar)
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

--Create particles for multiple explosions
mulExplParticles :: RandomGen g => g -> Float -> Float -> [Point] -> ([Particle], g)
mulExplParticles rndGen amount sp [] = ([], rndGen)
mulExplParticles rndGen amount sp (x:xs) = (fst thisExp ++ fst nextExp, snd nextExp)
                                         where
                                         thisExp = explosionParticles rndGen amount sp x
                                         nextExp = mulExplParticles (snd thisExp) amount sp xs