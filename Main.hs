module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Game as GG
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game as G2
import System.IO  
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 20
numSeeds = 4
width = 765 -- 51 * 15
height = 465 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100
tileSize = 15
maxTileX = 50
maxTileY = 27
dimPerFrame = 0.05
window = InWindow "Life" (width, height) (offset, offset)
background = black

data LifeGame = Game
  { 
  } deriving Show 

render :: LifeGame -> Picture 
render g = pictures [renderLevel g, 
                     renderDashboard g]

renderDashboard :: LifeGame -> Picture
renderDashboard g = G2.color white $ translate (-300) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ seedNum ++ wrapAround ++ dimmingEnabled ++ isPaused ++ fpsCount ++ aliveCells ++ isStable
  where
    fpsCount = " FPS: " ++ show fps
    aliveCells = " Alive: " ++ show (aliveCount g)
    isPaused  = " Pause: " ++ show (paused g)
    wrapAround  = " Wrap: " ++ show (wrapping g)
    dimmingEnabled  = " Dim: " ++ show (dimming g)
    seedNum  = " Seed: " ++ show ((currentLevelIdx g)+1) ++ ": " ++ show ((levelDescriptions g) !! (currentLevelIdx g))
    isStable = " Stable: " ++ show ((aliveCount g) == (prevAliveCount g))

-- Event handling
handleKeys :: Event -> LifeGame -> LifeGame
handleKeys (EventKey (Char 'p') Down _ _) g = togglePaused g
handleKeys (EventKey (Char 'w') Down _ _) g = toggleWrapping g
handleKeys (EventKey (Char 'd') Down _ _) g = toggleDimming g
handleKeys (EventKey (Char 'r') Down _ _) g = resetLevel g
handleKeys (EventKey (Char 'c') Down _ _) g = nextColour g
handleKeys (EventKey (Char 'n') Down _ _) g = nextLevel g
handleKeys _ game = game

update :: Float -> LifeGame -> LifeGame
update secs game
 | (paused game)               = game
 | otherwise                   = updateState $ updateLevel game

updateState :: LifeGame -> LifeGame
updateState g = g { aliveCount = (getAliveCount g), prevAliveCount = (aliveCount g) }

initTiles = do 
  let fileNames = map (\x -> "seed" ++ show x ++ ".txt") [1..numSeeds]
  fileContents <- mapM readFile fileNames
  descriptionContents <- readFile "seedDescriptions.txt"
  let all = map (levelToAlpha . words) fileContents
  let descriptions = lines descriptionContents
 
  let initialState = Game { allLevels = all, currentLevel = all !! 0, currentLevelIdx = 0, levelDescriptions = descriptions, paused = False, wrapping = True, dimming = False, colour = blue, aliveCount = 0, prevAliveCount = 0 }
  print all
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
