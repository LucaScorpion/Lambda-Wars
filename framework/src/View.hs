{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution (World{..})
    = pictures [
      pictures (map drawStars stars),
      translate (-fst cameraPos) (-snd cameraPos) 
      (pictures  $ (color violet (rectangleWire 2000 2000)) : drawPlayer player : pictures (map drawShip enemies) : (map drawParticle particles) ++ (map drawBullets bullets)),
      drawlives player,
      drawScore
      ]
      where
      drawPlayer (Ship{..}) = translate (fst sPos)(snd sPos) (pictures [rotate (-radToDeg sRot - 90) sSprite,
	                                                          if sInvuln > 0
                                                              then color blue $ circle  40
                                                              else blank])
      drawShip (Ship{..}) = translate (fst sPos)(snd sPos) (rotate ((-radToDeg sRot) - 90) sSprite)
      drawStars ((x,y),z) = translate
                            (worldWidth * x - 0.7 * (fst cameraPos * z))
                            (worldHeight * y - 0.7 * (snd cameraPos * z))
                            (color white $ circleSolid ((z + 0.3) * 1.9))
      drawBullets (Bullet{..}) = translate (fst bPos) (snd bPos) (color red $ circleSolid 4)
      drawlives (Ship{..}) = drawlife sLifes
                             where
                             drawlife 0 = blank
                             drawlife l = pictures[translate (horizontalResolution / 2 - l * 80) (verticalResolution / 2 - 40) sSprite, drawlife (l - 1)]
      drawScore = translate
                  (-horizontalResolution / 2 + 20)
                  (verticalResolution / 2 - 50)
				  (scale 0.3 0.3 (color white $ text $ "Score: " ++ show score))
      drawParticle (Particle{..}) = translate (fst pPos)(snd pPos) (color pColor $ circleSolid pSize)