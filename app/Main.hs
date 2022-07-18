module Main where
 
import UI (playGame)
import Diegod (Game(..))

import Control.Monad (when)
import Data.Monoid ((<>))
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

import Options.Applicative
import qualified System.Directory as D
import System.FilePath ((</>))

newtype Opts = Opts { score :: Bool }

opts :: Parser Opts
opts = Opts
  <$> switch (long "high-score" <> short 's' <> help "Print highscore and exit")

diegodHeader :: String
diegodHeader = "Diegod"

diegodFooter :: String
diegodFooter = "Controles - WS ou setas para mover. p para pausar, r para reinicar, q para sair"

fullOpts :: ParserInfo Opts
fullOpts = info (helper <*> opts) (fullDesc <> header diegodHeader <> footer diegodFooter)

main :: IO ()
main = do
  (Opts hs) <- execParser fullOpts
  when hs (getHighScore >>= printM >> exitSuccess)
  g <- playGame
  handleEndGame (_highscore g)

handleEndGame :: Int -> IO ()
handleEndGame s = do
  mhs <- getHighScore
  case mhs of
    Nothing -> newHighScore
    Just hs -> if s <= hs then justShowScore else newHighScore
  where
    justShowScore = putStrLn $ "Seu score final: " ++ show s
    newHighScore = do
      putStrLn $ "Seu novo maior score: " ++ show s
      setHighScore s

getHighScore :: IO (Maybe Int)
getHighScore = do
  lb <- getLeaderboardFile
  exists <- D.doesFileExist lb
  if exists
     then readMaybe <$> readFile lb
     else return Nothing


setHighScore :: Int -> IO ()
setHighScore s = do
  lb <- getLeaderboardFile
  writeFile lb (show s)


getLeaderboardFile :: IO FilePath
getLeaderboardFile = do
  xdg <- D.getXdgDirectory D.XdgData "diegod"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "leaderboard")


printM :: Show a => Maybe a -> IO ()
printM Nothing  = putStrLn "None"
printM (Just s) = print s
