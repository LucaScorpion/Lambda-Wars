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
      (pictures  $ (color violet (rectangleWire 2000 2000)) : drawShip player : pictures (map drawShip enemies) : (map drawBullets bullets))
      ]
      where
      drawShip (Ship{..}) = translate (fst sPos)(snd sPos) (pictures [rotate ((-radToDeg sRot) - 90) sSprite, color blue (circle 33)])
                            
							
      drawStars ((x,y),z) = translate
                            (worldWidth * x - 0.7 * (fst cameraPos * z))
                            (worldHeight * y - 0.7 * (snd cameraPos * z))
                            (color white $ circleSolid ((z + 0.3) * 1.9))
      drawBullets (Bullet{..}) = translate (fst bPos) (snd bPos) (color red $ circleSolid 4)