\begin{code}
module Test where

import           Control.Monad.State
import           Graphics.Htdp
import           System.Random

tromino :: Color -> Image
tromino c = aboveAlign low sq $ beside sq sq
 where
  sq :: Image
  sq = square 15 solid c

solveBase :: (Int, Int) -> State StdGen Image
solveBase p = do
  sg <- get
  let (r, g1) = randomR range sg
  let (g, g2) = randomR range g1
  let (b, g3) = randomR range g2
  let trom    = tromino $ makeColorI r g b 250
  put g3
  return $ rotation trom
 where
  rotation = case p of
    (0, 0) -> rotate 90
    (0, 1) -> rotate 180
    (1, 0) -> id
    _      -> rotate (-90)
  range = (0, 255)
  
solveBoard :: Int -> (Int, Int) -> State StdGen Image 
solveBoard 2 p = solveBase p
solveBoard n p = undefined

main :: IO ()
main = do g <- getStdGen
          let p = fst $ runState (solveBoard 2 (1, 1)) g
          drawImage p
\end{code}
