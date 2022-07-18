{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Diegod where

import Control.Monad.Random
import System.IO.Unsafe
import System.Random (randomRIO)
import qualified Data.Map as M
import Data.Ratio ((%))
import Control.Monad.Random
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..))
import System.Random (Random(..), randomRs, newStdGen)
import qualified Data.Sequence as S
import Data.Sequence(ViewR(..), ViewL(..), viewr, viewl, (|>))
import Data.Monoid (Any(..), getAny)

data Game = Game
  { _diegod         :: Diegod
  , _dir            :: Direction
  , _barriers       :: S.Seq Barrier
  , _barriersOtherType    :: S.Seq Barrier
  , _powerUpBox     :: S.Seq PowerUpBox
  , _dimns          :: [Dimension]
  , _positions      :: [Position]
  , _positionsMid   :: [PositionMid]
  , _level          :: Difficulty
  , _diffMap        :: DifficultyMap
  , _dead           :: Bool
  , _paused         :: Bool
  , _imortalO       :: Bool
  , _scoreMod       :: Int
  , _score          :: Score
  , _highscore      :: Score
  , _duckCountdown  :: Int
  , _imortalCountdown :: Int
  } deriving (Show)

type Score = Int
type Coord = V2 Int
type Diegod = [Coord]
type Barrier = [Coord]
type PowerUpBox = [Coord]
type BarrierBee = [Coord]
type Dimension = V2 Int

data DifficultyMap = DifficultyMap
  { _d0 :: DiffMod
  , _d1 :: DiffMod
  , _d2 :: DiffMod
  , _d3 :: DiffMod
  , _d4 :: DiffMod
  , _d5 :: DiffMod
  , _d6 :: DiffMod
  } deriving (Eq, Show)

data Direction =
    Up
  | Down
  | Duck
  | Still
  deriving (Eq, Show)

data Difficulty =
    D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  deriving (Eq, Ord, Show, Enum)

data PositionMid = Mid | Top deriving (Eq, Show)
data Position = Ground | Sky deriving (Eq, Show)

type WidthMod  = Int
type HeightMod = Int
type DistMod = [Int]
data DiffMod = DiffMod
  { _widthmod  :: WidthMod
  , _heightmod :: HeightMod
  , _distMod   :: DistMod
  } deriving (Eq, Show)

makeLenses ''DifficultyMap
makeLenses ''Game

gridWidth, gridHeight :: Int
gridWidth = 60
gridHeight = 40

standingDiegod :: Diegod
standingDiegod = [V2 7 13, V2 8 13, V2 9 13,
                V2 7 12, V2 8 12,
                V2 10 12,V2 7 11, V2 8 11,
                V2 9 11, V2 7 10, V2 8 10,
                V2 6 9, V2 7 9, V2 8 9,
                 V2 9 9,V2 6 8, V2 7 8, V2 8 8, V2 9 8,
                               V2 6 7, V2 7 7, V2 8 7, V2 9 7,
                               V2 6 6, V2 7 6, V2 8 6, V2 9 6,
                               V2 6 5, V2 9 5,                V2 7 5,
                                                                      V2 8 5,
                               V2 6 4, V2 7 4, V2 8 4, V2 9 4,
                               V2 6 3, V2 7 3, V2 8 3, V2 9 3,
                               V2 6 2,                 V2 9 2,
                               V2 6 1,                 V2 9 1,
                               V2 6 0, V2 7 0,         V2 9 0]



duckingDiegod :: Diegod
duckingDiegod = [V2 7 6, V2 8 6,
               V2 7 5, V2 8 5, V2 9 5,
       V2 6 4, V2 7 4, V2 8 4,
       V2 6 3, V2 7 3, V2 8 3,
       V2 6 2, V2 7 2, V2 8 2,
       V2 6 1, V2 7 1, V2 8 1,                           V2 12 1,
       V2 6 0, V2 7 0, V2 8 0, V2 9 0, V2 10 0, V2 11 0, V2 12 0]

maxHeight :: Int
maxHeight = 20

widthMin, widthMax, heightMin, heightMax :: Int
widthMin = 8
widthMax = 10
heightMin = 8
heightMax = 10

constScoreMod :: Int
constScoreMod = 2

levelAmount :: Score
levelAmount = 100

duckFrames :: Int
duckFrames = 10

imortalFrames :: Int
imortalFrames = 100

barrierCount :: Int
barrierCount = 0

scoreMap :: M.Map Int Difficulty
scoreMap = M.fromList $ zip [0 ..] [D0 ..]

step :: Game -> Game 
step g = fromMaybe g $ do
         guard $ not (g^.dead || g^.paused)
         return $ fromMaybe (step' g) (die g)

step' :: Game -> Game
step' = incDifficulty . setHighScore . incScore .  move  .  spawnPowerUpBox . spawnBarrier . move .
        decreaseImortalCountdown . deletePowerUpBox .deleteBarrier . adjustStanding . adjustDuckCountdown


adjustDuckCountdown :: Game -> Game
adjustDuckCountdown = setDirectionFromDuckCountdown . decreaseDuckCountdown

decreaseImortalCountdown :: Game -> Game
decreaseImortalCountdown g = if g^.imortalCountdown > 0 then g & imortalCountdown %~ subtract 1 else g

setDirectionFromDuckCountdown  :: Game -> Game
setDirectionFromDuckCountdown g = if (g^.duckCountdown <= 0) && (g^.dir == Duck) then g & dir .~ Still else g

setDirectionFromImortalCountdown :: Game -> Game
setDirectionFromImortalCountdown g = if (g^.imortalCountdown <= 0) then g & imortalO .~ False else g

decreaseDuckCountdown :: Game -> Game
decreaseDuckCountdown g = if g^.duckCountdown > 0 then g & duckCountdown %~ subtract 1 else g

incScore :: Game -> Game
incScore g = case g^.scoreMod of
  0 -> g & score %~ (+1) & scoreMod %~ incAndMod
  _ -> g & scoreMod %~ incAndMod
  where incAndMod x = (x + 1) `mod` constScoreMod

setHighScore :: Game -> Game
setHighScore g = if g^.score > g^.highscore
                   then g & highscore .~ (g^.score)
                   else g

getDiffMod :: Game -> DiffMod
getDiffMod g = case g^.level of
  D0 -> g^.diffMap.d0
  D1 -> g^.diffMap.d1
  D2 -> g^.diffMap.d2
  D3 -> g^.diffMap.d3
  D4 -> g^.diffMap.d4
  D5 -> g^.diffMap.d5
  D6 -> g^.diffMap.d6

setDiffMod :: DiffMod -> Game -> Game
setDiffMod dm g = case g^.level of
  D0 -> g & diffMap.d0 .~ dm
  D1 -> g & diffMap.d1 .~ dm
  D2 -> g & diffMap.d2 .~ dm
  D3 -> g & diffMap.d3 .~ dm
  D4 -> g & diffMap.d4 .~ dm
  D5 -> g & diffMap.d5 .~ dm
  D6 -> g & diffMap.d6 .~ dm

scoreToDiff :: Score -> Difficulty
scoreToDiff sc = let l = sc `div` levelAmount
                    in fromMaybe D6 (M.lookup l scoreMap)

incDifficulty :: Game -> Game
incDifficulty g = g & level .~ scoreToDiff (g^.score)



die :: Game -> Maybe Game
die g = do
  guard $ (die' g) 
  return $ g & dead .~ (True && not (g^.imortalO) )

die' :: Game -> Bool
die' g = let nextD = nextDiegod g
             nextB = nextBarriers g
          in getAny $ foldMap (Any . flip inBarriers nextB) nextD

nextDiegod :: Game -> Diegod
nextDiegod g = moveDiegod g^.diegod

nextBarriers :: Game -> S.Seq Barrier
nextBarriers g = moveBarriers g^.barriers

nextOtherTypeBarriers :: Game -> S.Seq Barrier
nextOtherTypeBarriers g = moveBarriers g^.barriersOtherType

nextPowerUpBoxes :: Game -> S.Seq PowerUpBox
nextPowerUpBoxes g = movePowerUpBoxes g^.powerUpBox

adjustStanding :: Game -> Game
adjustStanding g = let d = g^.dir in
  case d of
    Duck -> if isDiegodBottom g then g & diegod .~ duckingDiegod else g
    _    -> if isDiegodBottom g then g & diegod .~ standingDiegod else g

move :: Game -> Game
move = moveDiegod . moveBarriers . movePowerUpBoxes

moveDiegod :: Game -> Game
moveDiegod g = let d = g^.dir in
  case d of
    Up   -> if shouldStopDiegod d g then setDiegodDir Down g else moveDiegod' 1 g
    Down -> if shouldStopDiegod d g then setDiegodDir Still g else
              (let gNext = moveDiegod' (-1) g in
                if isDiegodBottom gNext then setDiegodDir Still gNext else gNext)
    Duck -> if shouldStopDiegod d g then g else moveDiegod' (-1) g
    _    -> g

moveDiegod' :: Int -> Game -> Game
moveDiegod' amt g = g & diegod %~ fmap (+ V2 0 amt)

setDiegodDir :: Direction -> Game -> Game
setDiegodDir d g = g & dir .~ d

shouldStopDiegod :: Direction -> Game -> Bool
shouldStopDiegod d g = case d of
  Down -> isDiegodBottom g
  Duck -> isDiegodBottom g
  Up   -> diegodBottom g >= maxHeight
  _    -> False

isDiegodBottom :: Game -> Bool
isDiegodBottom g = diegodBottom g <= 13

diegodBottom :: Game -> Int
diegodBottom g =
  let d = g^.diegod
      (V2 _ y) = head d
  in y

moveBarriers :: Game -> Game
moveBarriers g = g & barriers %~ fmap moveBarrier

moveBarrier :: Barrier -> Barrier
moveBarrier = fmap (+ V2 (-1) 0)

moveBarrierBee :: Barrier -> Barrier
moveBarrierBee = fmap (+ V2 (-3) 0)

movePowerUpBoxes :: Game -> Game
movePowerUpBoxes g = g & powerUpBox %~ fmap movePowerUpBox

movePowerUpBox :: PowerUpBox -> PowerUpBox
movePowerUpBox = fmap (+ V2 (-1) 0)

deleteBarrier :: Game -> Game
deleteBarrier g =
  case viewl $ g^.barriers of
    EmptyL  -> g
    a :< as -> let x = getBarrierRightmost a in
                 (if x <= 0 then g & barriers .~ as else g)

deletePowerUpBox :: Game -> Game
deletePowerUpBox g =
  case viewl $ g^.powerUpBox of
    EmptyL -> g
    a :< as -> let x = getPowerUpBoxRightMost a in
                    (if x <= 0 then g & powerUpBox .~ as else g)

spawnBarrierOther :: Game -> Game
spawnBarrierOther g =
  let (DiffMod wm hm (d:ds)) = getDiffMod g
      newDiffMod = DiffMod wm hm ds
  in case viewr $ g^.barriers of
    EmptyR -> addRandomBarrierOther g
    _ :> a -> let x = getBarrierLeftmost a in
                if (gridWidth - x) > d then setDiffMod newDiffMod (addRandomBarrierOther g) else g

spawnBarrier :: Game -> Game
spawnBarrier g =
  let (DiffMod wm hm (d:ds)) = getDiffMod g
      newDiffMod = DiffMod wm hm ds
  in case viewr $ g^.barriers of
    EmptyR -> addRandomBarrier g
    _ :> a -> let x = getBarrierLeftmost a in
                if (gridWidth - x) > d then setDiffMod newDiffMod (addRandomBarrier g) else g


spawnPowerUpBox :: Game -> Game
spawnPowerUpBox g =
  let (DiffMod wm hm (d:ds)) = getDiffMod g
      newDiffMod = DiffMod wm hm ds
  in case viewr $ g^.powerUpBox of
    EmptyR -> addPowerUpBox g
    _ :> a -> let x = getPowerUpBoxLeftMost a in
            if (gridWidth - x) > d then setDiffMod newDiffMod (addPowerUpBox g) else g

getPowerUpBoxLeftMost :: PowerUpBox -> Int
getPowerUpBoxLeftMost [] = 0
getPowerUpBoxLeftMost (V2 x _:_) = x

getPowerUpBoxRightMost :: PowerUpBox -> Int
getPowerUpBoxRightMost [] = gridWidth
getPowerUpBoxRightMost b = let (V2 x _) = last b in x

getBarrierLeftmost :: Barrier -> Int
getBarrierLeftmost [] = 0
getBarrierLeftmost (V2 x _:_) = x

getBarrierRightmost :: Barrier -> Int
getBarrierRightmost [] = gridWidth
getBarrierRightmost b = let (V2 x _) = last b in x


addPowerUpBox :: Game -> Game
addPowerUpBox g = 
  let (V2 w h:rest) = g^.dimns
      (DiffMod wm hm _) = getDiffMod g
      newPowerUpBox = makePowerUpBox (V2 (min w wm) (min h hm)) 3
  in g & powerUpBox %~ (|> newPowerUpBox) & dimns .~ rest

addRandomBarrier :: Game -> Game
addRandomBarrier g =
  let (p:ps) = g^.positions
  in case p of
    Sky    -> addRandomSkyBarrier g & positions .~ ps
    Ground -> addRandomGroundBarrier g & positions .~ ps

addRandomBarrierOther :: Game -> Game
addRandomBarrierOther g =
  let (p:ps) = g^.positionsMid
  in case p of
    Mid    -> addRandomMidBarrier g & positionsMid .~ ps
    Top -> addRandomMidBarrier g & positionsMid .~ ps

addRandomGroundBarrier :: Game -> Game
addRandomGroundBarrier g =
  let (V2 w h:rest) = g^.dimns
      (DiffMod wm hm _) = getDiffMod g
      newBarrier = makeBarrier g (V2 (min w wm) (min h hm)) 6
  in g & barriers %~ (|> newBarrier) & dimns .~ rest

addRandomSkyBarrier :: Game -> Game
addRandomSkyBarrier g =
  let (V2 w h:rest) = g^.dimns
      (DiffMod wm hm _) = getDiffMod g
      newBarrier = makeBarrier g (V2 (min w wm) (min h hm)) 15
  in g & barriers %~ (|> newBarrier) & dimns .~ rest

addRandomMidBarrier :: Game -> Game
addRandomMidBarrier g =
  let (V2 w h:rest) = g^.dimns
      (DiffMod wm hm _) = getDiffMod g
      newBarrier = makeBarrierOther g (V2 (min w wm) (min h hm)) 2
  in g & barriers %~ (|> newBarrier) & dimns .~ rest

getRandomN :: Int -> Int
getRandomN seed = head (getRandomLs 1 seed)

getRandomLs :: Int -> Int -> [Int]
getRandomLs n seed = randomNumbersS
                       where
                         randomNumbersU = (take n (randoms (mkStdGen seed) :: [Int]))
                         randomNumbersS = sortNegativeLs randomNumbersU

sortNegativeLs :: [Int] -> [Int]
sortNegativeLs [] = []
sortNegativeLs (x:xs)
 | x<0 = (x * (-1)) : sortNegativeLs xs
 | otherwise = x : sortNegativeLs xs

pedagio :: [Coord]
pedagio = [V2 gridWidth 3, V2 gridWidth 2, V2 gridWidth 1, V2 gridWidth 0]

corda :: Int -> [Coord]
corda altura_corda = [V2 (gridWidth+1) (altura_corda+1), V2 (gridWidth+2) (altura_corda+1),
                      V2 gridWidth altura_corda, V2 (gridWidth+3) (altura_corda),
                      V2 (gridWidth+1) (altura_corda-1), V2 (gridWidth+2) (altura_corda-1), V2 (gridWidth+4) (altura_corda-1),
                      V2 (gridWidth+5) (altura_corda-2)]

powerUpBoxSEQ :: Int -> [Coord]
powerUpBoxSEQ altura = [V2 (gridWidth+1) (altura+1),
                     V2 gridWidth altura, V2 (gridWidth + 1) altura, V2 (gridWidth + 2) altura,
                     V2 (gridWidth+1) (altura-1)]

pool :: [Coord]
pool = [V2 gridWidth 0,V2 (gridWidth+1) 0,V2 (gridWidth+2) 0,V2 (gridWidth+3) 0,V2 (gridWidth+4) 0]

bee :: Int -> [Coord]
bee altura_abelha = [V2 gridWidth altura_abelha, V2 (gridWidth+1) altura_abelha, V2 (gridWidth+3) altura_abelha, V2 (gridWidth+5) altura_abelha,
                    V2 (gridWidth-1) (altura_abelha-1), V2 (gridWidth) (altura_abelha-1), V2 (gridWidth+1) (altura_abelha-1), V2 (gridWidth+2) (altura_abelha-1), V2 (gridWidth+4) (altura_abelha-1),  V2 (gridWidth+6) (altura_abelha-1), V2 (gridWidth+7) (altura_abelha-1),
                    V2 (gridWidth-2) (altura_abelha-2), V2 (gridWidth) (altura_abelha-2), V2 (gridWidth+2) (altura_abelha-2), V2 (gridWidth+4) (altura_abelha-2), V2 (gridWidth+6) (altura_abelha-2), V2 (gridWidth+8) (altura_abelha-2),
                    V2 (gridWidth-2) (altura_abelha-3), V2 (gridWidth-1) (altura_abelha-3), V2 (gridWidth) (altura_abelha-3), V2 (gridWidth+1) (altura_abelha-3), V2 (gridWidth+2) (altura_abelha-3), V2 (gridWidth+4) (altura_abelha-3), V2 (gridWidth+6) (altura_abelha-3), V2 (gridWidth+9) (altura_abelha-3),
                    V2 (gridWidth-2) (altura_abelha-4), V2 (gridWidth-1) (altura_abelha-4), V2 (gridWidth+1) (altura_abelha-4), V2 (gridWidth+2) (altura_abelha-4), V2 (gridWidth+4) (altura_abelha-4), V2 (gridWidth+6) (altura_abelha-4), V2 (gridWidth+8) (altura_abelha-4),
                    V2 (gridWidth-1) (altura_abelha-5), V2 (gridWidth) (altura_abelha-5), V2 (gridWidth+1) (altura_abelha-5), V2 (gridWidth+2) (altura_abelha-5), V2 (gridWidth+4) (altura_abelha-5), V2 (gridWidth+6) (altura_abelha-5), V2 (gridWidth+7) (altura_abelha-5),
                    V2 gridWidth (altura_abelha-6), V2 (gridWidth+1) (altura_abelha-6), V2 (gridWidth+3) (altura_abelha-6), V2 (gridWidth+5) (altura_abelha-6)]

makeBarrier :: Game -> Dimension -> Int -> Barrier
makeBarrier g (V2 w h) altura_abelha =
   [bee ((mod (altura_abelha + g^.score) 15) + 15), pedagio, pool, corda (altura_abelha - 4), bee altura_abelha] !! (mod (g^.score) 4)

makeBarrierOther :: Game -> Dimension -> Int -> Barrier
makeBarrierOther g (V2 w h) altura_abelha =
   [pool, corda altura_abelha]!!(mod ((getRandomLs 2 altura_abelha) !! 0) 2)

makePowerUpBox :: Dimension -> Int -> PowerUpBox
makePowerUpBox (V2 w h) altura =
   powerUpBoxSEQ altura

inPowerUpBoxes :: Coord -> S.Seq PowerUpBox -> Bool
inPowerUpBoxes c ps = getAny $ foldMap (Any . inPowerUpBox c) ps

inPowerUpBox :: Coord -> PowerUpBox -> Bool
inPowerUpBox c p = c `elem` p

inBarriers :: Coord -> S.Seq Barrier -> Bool
inBarriers c bs = getAny $ foldMap (Any . inBarrier c) bs

inBarrier :: Coord -> Barrier -> Bool
inBarrier c b = c `elem` b

weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weights = evalRand m gen
    where m = sequence . repeat . fromList $ weights

initGame :: Score -> IO Game
initGame hs = do
  dimensions      <- randomRs (V2 widthMin heightMin, V2 widthMax heightMax) <$> newStdGen
  randomPositions <- flip weightedList ((Sky, 1 % 4) : replicate 3 (Ground, 1 % 4)) <$> newStdGen
  randomPositionsMid <- flip weightedList ((Top, 1 % 4) : replicate 3 (Mid, 1 % 4)) <$> newStdGen
  dMap            <- difficultyMap
  let g = Game { _diegod      = standingDiegod
               , _dir       = Still
               , _barriers  = S.empty
               , _barriersOtherType = S.empty
               , _powerUpBox = S.empty
               , _dimns     = dimensions
               , _positions = randomPositions
               , _positionsMid = randomPositionsMid
               , _level     = D0
               , _diffMap   = dMap
               , _paused    = False
               , _dead      = False
               , _imortalO   = False
               , _scoreMod  = 0
               , _score     = 0
               , _highscore = hs
               , _duckCountdown = -1
               , _imortalCountdown = -1
               }
  return g

difficultyMap :: IO DifficultyMap
difficultyMap = do
  dists0 <- randomRs (200, 100) <$> newStdGen
  dists1 <- randomRs (17, 22) <$> newStdGen
  dists2 <- randomRs (16, 20) <$> newStdGen
  dists3 <- randomRs (15, 20) <$> newStdGen
  dists4 <- randomRs (12, 18) <$> newStdGen
  distsHardest <- randomRs (12, 16) <$> newStdGen
  return $ DifficultyMap
    (DiffMod 20 3 dists0)
    (DiffMod 1 2 dists1)
    (DiffMod 2 2 dists2)
    (DiffMod 2 3 dists3)
    (DiffMod 3 3 dists4)
    (DiffMod 3 4 dists4)
    (DiffMod 3 4 distsHardest)



instance Random a => Random (V2 a) where
  randomR (V2 x1 y1, V2 x2 y2) g =
    let (x, g')  = randomR (x1, x2) g
        (y, g'') = randomR (y1, y2) g'
     in (V2 x y, g'')
  random g =
    let (x, g')  = random g
        (y, g'') = random g'
     in (V2 x y, g'')

--imortalize :: Game -> Game
--imortalize g = do
--  guard $ imortalize' g
--  return $ g & imortalO .~ True
--
--imortalize' :: Game -> Bool
--imortalize' g = let nextD = nextDiegod g
--                    nextP = nextPowerUpBoxes g
--                in getAny $ foldMap (Any . flip inPowerUpBoxes nextP) nextD