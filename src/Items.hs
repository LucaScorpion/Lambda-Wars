{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}

module Items where

import Data.List
import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Geometry

import Model

--Create an item
createItem :: Point -> Int -> [Picture] -> Item
createItem pos 0 pics = Multiplier {iPos = pos , iPicture = pics !! 0 , iTimer = 150}
createItem pos 1 pics = Invulnerable {iPos = pos, iPicture = pics !! 1, iTimer = 100}
createItem pos 2 pics = Life {iPos = pos, iPicture = pics !! 2, iTimer = 200}

--Update all items
updateItems :: Float -> [Item] -> [Item]
updateItems _ []        = [] 
updateItems time (x:xs) = if isJust thisItem then fromJust thisItem : otherItems else otherItems
                          where
                          otherItems = updateItems time xs
                          thisItem = updateItem time x

--Update an item
updateItem :: Float -> Item -> Maybe Item
updateItem time item@(Multiplier{..})   = if iTimer > 0 then Just (item{ iTimer = iTimer - time }) else Nothing
updateItem time item@(Invulnerable{..}) = if iTimer > 0 then Just (item{ iTimer = iTimer - time }) else Nothing
updateItem time item@(Life{..})         = if iTimer > 0 then Just (item{ iTimer = iTimer - time }) else Nothing

--Get the item position
getItempos (Multiplier{iPos}) = iPos
getItempos (Invulnerable{iPos}) = iPos
getItempos (Life{iPos}) = iPos

--Apply an item powerup
applyItem ship@(Ship{..}) Multiplier{}   = ship { sMultiply = sMultiply + 1 }
applyItem ship@(Ship{..}) Invulnerable{} = ship { sInvuln = 5 }
applyItem ship@(Ship{..}) Life{}         = ship { sLifes = min (sLifes + 1) 5 }

--Draw an item
drawItem (Multiplier{..})   = translate (fst iPos) (snd iPos) iPicture
drawItem (Invulnerable{..}) = translate (fst iPos) (snd iPos) iPicture
drawItem (Life{..})         = translate (fst iPos) (snd iPos) iPicture