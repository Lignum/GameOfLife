{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified SDL 
import qualified SDL.Video as SDL
import qualified SDL.Event as SDL
import qualified SDL.Vect as SDL
import qualified SDL.Init as SDL
import SDL (($=))

import Data.Foldable (foldl')

import System.Random

import qualified Data.Vector as V 

import Control.Monad.State.Strict
import Data.Traversable (for)

data Tile
  = Tile { tileOccupied :: Bool
         , tileX :: Int
         , tileY :: Int }

data Board
  = Board { boardTiles :: V.Vector Tile
          , boardWidth :: Int
          , boardHeight :: Int }

data GameState
  = GameState { gameBoard :: Board }

data Components
  = Components { window :: SDL.Window 
               , renderer :: SDL.Renderer } 

initGame :: IO Components  
initGame = do
  SDL.initializeAll

  let wcfg = SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 800 800 }
  window <- SDL.createWindow "game of haskl" wcfg 
  renderer <- SDL.createRenderer window 0 SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedRenderer
                                                             , SDL.rendererTargetTexture = False }
  SDL.showWindow window
  pure $ Components window renderer

handleEvent :: SDL.EventPayload -> IO ()
handleEvent e = pure ()

boardNew :: Int -> Int -> Board
boardNew w h = Board tiles w h where
  tiles = flip fmap (V.fromList [0..(w*h)-1]) $
    \i -> let tx = i `mod` w
              ty = i `div` h
             in Tile False tx ty 

randomBoard :: Int -> Int -> IO Board
randomBoard w h = do 
  let board = boardNew w h
  tiles <- forM (boardTiles board) $ \t -> do g <- newStdGen
                                              pure $ t { tileOccupied = head $ randoms g :: Bool }
  pure $ board { boardTiles = tiles }


boardTileWidth, boardTileHeight :: Int
(boardTileWidth, boardTileHeight) = (4, 4)

boardGetTile :: Board -> (Int, Int) -> Maybe Tile
boardGetTile Board{..} (x, y) = boardTiles V.!? i where
    i = y * boardWidth + x

boardIsOccupied :: Board -> (Int, Int) -> Bool
boardIsOccupied b (x, y) = case boardGetTile b (x, y) of
                             Just t  -> tileOccupied t
                             Nothing -> False

boardGetNeighbourCount :: Board -> (Int, Int) -> Int
boardGetNeighbourCount b (x, y) = foldl' (\a (x, y) -> a + boardGetCount b (x, y)) 0 absNeighbours
  where
    neighbours = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]
    absNeighbours = flip fmap neighbours $ \(i, j) -> (x + i, y + j)
    boardGetCount b (x, y) = if boardIsOccupied b (x, y) then 1 else 0

renderRectangle :: SDL.Renderer -> Int -> Int -> Int -> Int -> IO ()
renderRectangle r x y w h = 
  SDL.fillRect r $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) $ SDL.V2 (fromIntegral w) (fromIntegral h)

renderBoard :: Components -> Board -> IO ()
renderBoard c b = do
  let r = renderer c
  rects <- forM (boardTiles b) $ \t -> let tx   = tileX t * boardTileWidth 
                                           ty   = tileY t * boardTileHeight
                                          in do SDL.rendererDrawColor r $= if tileOccupied t then SDL.V4 0 0 0 255
                                                                                             else SDL.V4 255 255 255 255
                                                renderRectangle r tx ty boardTileWidth boardTileHeight  
  
  pure ()

updateTile :: Board -> Tile -> Tile
updateTile b t 
  | tileOccupied t = if | nc < 2    -> t { tileOccupied = False } -- Underpopulation
                        | nc > 3    -> t { tileOccupied = False } -- Overpopulation
                        | otherwise -> t                          -- All good
  | otherwise      = if | nc == 3   -> t { tileOccupied = True }  -- Reproduce 
                        | otherwise -> t                          -- Nothing happens
  where
    nc = boardGetNeighbourCount b (tileX t, tileY t) 

updateBoard :: Board -> Board
updateBoard b = b { boardTiles = fmap (updateTile b) (boardTiles b) }

updateGame :: Components -> StateT GameState IO ()
updateGame c = do
  let r = renderer c
  SDL.rendererDrawColor r $= SDL.V4 0 127 255 255
  SDL.clear r 
  board <- gets gameBoard
  let nboard = updateBoard board
     in do liftIO $ renderBoard c nboard
           modify $ \s -> s { gameBoard = nboard }
  SDL.present r
  SDL.delay 16 

gameLoop :: Components -> StateT GameState IO ()
gameLoop c = do
  events <- liftIO SDL.pollEvents
  if | null events -> updateGame c >> gameLoop c
     | otherwise   -> do quitSignals <- forM events $ \event -> case SDL.eventPayload event of
                                                                  SDL.QuitEvent -> pure True
                                                                  e             -> liftIO $ handleEvent e >> pure False
                         if | or quitSignals -> pure ()
                            | otherwise      -> updateGame c >> gameLoop c

destroyGame :: Components -> IO ()
destroyGame c = do
  SDL.destroyRenderer $ renderer c
  SDL.destroyWindow $ window c 
  SDL.quit

main = do
  board <- randomBoard 200 200 
  c <- initGame
  runStateT (gameLoop c) $ GameState board 
  destroyGame c
