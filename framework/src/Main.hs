{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns                        #-}
{-# LANGUAGE ViewPatterns                                                    #-}

-- http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/syntax-extns.html
-- http://hackage.haskell.org/package/gloss-1.8.1.2/docs/Graphics-Gloss.html

module Main where

import Control.Applicative

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment    (getArgs)

import Graphics.Gloss                                              {- 1.8.2.1 -}

import Config
import Model
import View
import Controller

-- | Main

main :: IO ()
main = do
    playerShip <- loadBMP "lambda-wars-tmp/Resources/kspaceduel64.bmp"
    insectShip <- loadBMP "lambda-wars-tmp/Resources/insectShip64.bmp"
    args <- getArgs
    time <- round <$> getPOSIXTime
    let initial'        = initial time [playerShip, insectShip]
    let (w, h, display) = chooseDisplay args
    let background      = black
    let fps             = 60
    play display background fps initial' (draw w h) eventHandler timeHandler

-- | Choose a display mode. Note that the resolution of a full screen mode
--   should likely match the resolution of your monitor exactly.
chooseDisplay :: [String] -> (Float, Float, Display)
chooseDisplay []
    = ( defaultHorizontalResolution, defaultVerticalResolution
      , InWindow "Lambda Wars"
                 (round defaultHorizontalResolution
                 ,round defaultVerticalResolution  )
                 (100,100)
      )
chooseDisplay [read -> horizontal, read -> vertical]
    = ( fromIntegral horizontal, fromIntegral vertical
      , FullScreen (horizontal, vertical)
      )
