{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Diegod
import Controls

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Lens.Micro ((^.))

data Tick = Tick

type Name = ()

data Cell = Diegod | Barrier | BarrierBee | Empty | PowerUp | PowerUpBox


minFrameRate :: Int
minFrameRate = 30000


app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }


handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ handleUp g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ handleDown g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ handleUp g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ handleDown g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ pause g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) =
  liftIO (writeIORef counter 0 >> initGame (g^.highscore)) >>= continue
handleEvent g _ = continue g

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         , padTop (Pad 2) $ drawPause (g ^. paused) (g ^. dead)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " score ")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawPause :: Bool -> Bool -> Widget Name
drawPause isPaused isDead= 
  if (isPaused && (not isDead))
    then withAttr pausedAttr $ C.hCenter $ str "pausado"
  else 
    if(not isDead)
      then withAttr runAttr $ C.hCenter $ str "corra!"
    else emptyWidget

drawGameOver :: Bool -> Widget Name
drawGameOver isDead =
  if isDead
     then withAttr gameOverAttr $ C.hCenter $ str "game over"
  else withAttr runAttr $ C.hCenter $ str "sobreviva!"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " O caminho da Goiaba ")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [gridHeight - 1,gridHeight - 2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..gridWidth - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` g^.diegod           = Diegod
      | inBarriers c (g^.barriers) = Barrier
      | inBarriers c (g^.barriersOtherType) = Barrier
      | inPowerUpBoxes c (g^.powerUpBox) = PowerUpBox
      | otherwise                  = Empty

drawCell :: Cell -> Widget Name
drawCell Diegod    = withAttr diegodAttr cw
drawCell Barrier = withAttr barrierAttr cw
drawCell BarrierBee = withAttr barrierBeeAttr cw
drawCell PowerUp = withAttr powerUpAttr cw
drawCell PowerUpBox = withAttr powerUpBoxAttr cw
drawCell Empty   = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
 [ (diegodAttr, V.yellow `on` V.red)
 , (imortalDiegodAttr, V.yellow `on` V.yellow)
 , (barrierAttr, V.blue `on` V.yellow)
 , (barrierBeeAttr, V.red `on` V.red)
 , (gameOverAttr, fg V.red `V.withStyle` V.bold)
 , (powerUpBoxAttr, V.white `on` V.white)
 , (powerUpAttr, V.white `on` V.white)
 , (pausedAttr, fg V.white `V.withStyle` V.bold)
 , (runAttr, fg V.white `V.withStyle` V.bold)
 ]

imortalDiegodAttr :: AttrName
imortalDiegodAttr = "imortalDiegod"

runAttr :: AttrName
runAttr = "corra"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

pausedAttr :: AttrName
pausedAttr = "paused"

powerUpBoxAttr :: AttrName
powerUpBoxAttr = "powerUpBox"

diegodAttr, barrierAttr, barrierBeeAttr, emptyAttr :: AttrName
diegodAttr = "diegodAttr"
barrierAttr = "barrierAttr"
barrierBeeAttr = "barrierBeeAttr"
emptyAttr = "emptyAttr"
powerUpAttr = "powerUpAttr"


counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)

playGame :: IO Game
playGame = do
  chan <- newBChan 10
  forkIO $ forever $ do
    modifyIORef counter (+1)
    c' <- readIORef counter
    writeBChan chan Tick
    threadDelay (max (65000 - c' * 10) 35000)
  g <- initGame 0
  customMain (V.mkVty V.defaultConfig) (Just chan) app g