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
      (pictures  $ (color violet (rectangleWire 2000 2000)) :
                    drawPlayer player :
                    drawShield player :
                    map drawShip enemies ++
                    map drawParticle exhaustP ++
                    map drawParticle explosionP ++
                    map drawBullets bullets ++ map drawItem items),
      drawlives player,
      drawScore player
      ]
      where
      drawPlayer player@(Ship {..}) = if sLifes > 0
                                      then translate (fst sPos)(snd sPos) (rotate (-radToDeg sRot - 90) sSprite)
                                      else blank
      drawShield (Ship {..}) = if sInvuln > 0 && sLifes > 0
                               then translate (fst sPos)(snd sPos) (color blue $ circle  40)
                               else blank
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
      drawScore (Ship{sScore}) = translate
                                 (-horizontalResolution / 2 + 20)
                                 (verticalResolution / 2 - 50)
				                 (scale 0.3 0.3 (color white $ text $ "Score: " ++ show sScore))
      drawParticle (Particle{..}) = translate (fst pPos)(snd pPos) (color pColor $ circleSolid pSize)
	  
      drawItem (Multiplier{..}) = translate (fst iPos) (snd iPos) iPicture
      drawItem (Invulnerable{..}) = translate (fst iPos) (snd iPos) iPicture