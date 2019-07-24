Hey there!

This program demonstrates an inductive solution to the tromino tiling
problem with the htdp-image library. Before we start, let's get some
unecessary work out of the way.

This program will use Graphics.Htdp to present the solution, System.Random
for random number generators, Control.Monad.State to make it easy for
our implementation to carry around the random number generator, and finally,
System.Environment for command line interaction.

\begin{code}
module Main where
import           Control.Monad.State
import           Graphics.Htdp
import           System.Random
import           System.Environment

\end{code}

The tromino tiling problem is stated as follows:
Given a 2^n x 2^n tile board, is it possible to fill all but one
tile by an arrangement of trominoes (an L-shaped arrangement of three squares)
[http://www.itu.dk/~carsten/courses/f02/assignments/hw5/hw5.html]

Well, let's create a tromino as it is described in the problem along with the tile size:

\begin{code}

tileSize :: Float -- Since htdp library expects float
tileSize = 15

-- | An L shaped tromino.
tromino :: Color -> Image
tromino c = aboveAlign low sq $ beside sq sq
 where
  sq :: Image
  sq = square tileSize solid c
\end{code}

While we are at it, let's also make a function that creates a random color.

\begin{code}
colorR :: State StdGen Color
colorR = do
  g0 <- get
  let (r, g1) = randomR range g0
  let (g, g2) = randomR range g1
  let (b, g3) = randomR range g2
  put g3
  return $ makeColorI r g b 250
  where range = (0, 250)

\end{code}

Since we are solving this problem inductively, lets think of our base case.

The smallest possible board that can be solved in a way described above is a four by four
board (which is 2^1 x 2^1). The following are the 4 possible solutions for a given tromino.

\begin{code}
topLeftEmpty :: Image -> Image
topLeftEmpty = rotate 90

topRightEmpty :: Image -> Image
topRightEmpty = id

bottLeftEmpty :: Image -> Image
bottLeftEmpty =  rotate 180

bottRightEmpty :: Image -> Image
bottRightEmpty =  rotate (270)
\end{code}

So, the claim holds true for the base case.

Since we are making a program, we will actually need to know what tile our user selected
to be left out. So, lets make a function that gives us a solution for a 2 x 2 board. To
make our solution look pretty, we will take advantage of random numbers for random colors,
and the state monad to carry around the generator.

\begin{code}
--            X    Y
type Posn = (Int, Int) -- Zero indexed position on a board in screen coordinate system

solveBase :: Posn -> State StdGen Image
solveBase p = do
  c <- colorR
  let trom = tromino $ c
  return $ rotation trom
 where
  rotation = case p of
    (0, 0) -> topLeftEmpty
    (0, 1) -> bottLeftEmpty
    (1, 0) -> topRightEmpty
    _      -> bottRightEmpty
\end{code}

All's good so far. Now, for the inductive step.

Assume that the claim holds for a 2^k x 2^k board.

We need to show that the claim also holds for 2^(k+1) x 2^(k+1) board.

Notice that:

2^(k+1) x 2^(k+1) = 2x2^k x 2*2^k
                  = 4 x (2^k x 2^k)
                  = (2^k x 2^k) + (2^k x 2^k) + (2^k x 2^k) + (2^k x 2^k)

So, we have four copies of (2^k x 2^k) boards. And from our inductive hypothesis,
we know that we can solve a (2^k x 2^k) board. So, all we have to figure out is
how to combine these four boards to complete our solution.

Let's name these four boards A, B, C, and D. Assume that the position user selected is
in board A.

From our inductive hypothesis, we know that we can solve board A, meaning that we can
fill up every tile but the one our user selected. So board A is good to go.

But what about board B, C, and D? We know that our inductive hypothesis promises to fill
every tile but one, so that means we need to do something clever with the remaining three
boards (and tiles).

For easiness, assume that the four boards are setup as follows:

 A   |   B
     |
-----------
     |
 C   |   D

All we need now is a stategy to solve B, C, and D so that we can solve the whole board.

The idea is to choose the tiles touching the center of the board.

In board B, we fill all but the B's bottom left tile. In C, all but C's top right. And finally,
in D, all but D's top left. This gives us a tromino shaped hole in the center, which we can fill
ourselves!

Now lets write the idea above in a programmatic way.

First, let's make a function that takes and int `n` and a position `p` in a (2^n x 2^n) board,
and returns two things:
1. list of four positions that don't need to be filled in the four boards that are created from
   a bigger board (as shown in above).
2. The tromino rotation so that it fits in the middle of the four boards

\begin{code}
findPosnsAndRotation :: Int -> Posn -> ([Posn], Image -> Image)
findPosnsAndRotation n (x, y)
  | x <  divLen && y <  divLen = ([(x, y), bBoardBL, cBoardTR, dBoardTL], topLeftEmpty)
  | x >= divLen && y <  divLen = ([aBoardBR, (x - divLen, y), cBoardTR, dBoardTL], topRightEmpty)
  | x <  divLen && y >= divLen = ([aBoardBR, bBoardBL, (x, y - divLen), dBoardTL], bottLeftEmpty)
  | x >= divLen && y >= divLen = ([aBoardBR, bBoardBL, cBoardTR, (x - divLen, y - divLen)], bottRightEmpty)
  where aBoardBR = (pred divLen , pred divLen)
        bBoardBL = (0           , pred divLen)
        cBoardTR = (pred divLen , 0)
        dBoardTL = (0           , 0)

        divLen = len `div` 2
        len = 2 ^ n
\end{code}

The idea above is that whenever we are in a region where either `x` or `y` is greater than `divLen`,
we convert the position so that it's relative to the board it belongs in.

For example, in a 2^2 x 2^2 (or 4 x 4 board), `divLen` = 2, and position (3, 3) belings in the bottom
right corner. So, if we wanted (3, 3) to be shown in a (2 x 2) board, that would be (3 - 2, 3 - 2)

Similarly, in variables like `aBoardBR`, we are finding the corner positions, which is some combination
of 0 and `divLen`.

Now that we our helper function ready, let's right a function that solves a bigger board by dividing
it into four boards, as we did above.

\begin{code}
solveInd :: Int -> Posn -> State StdGen Image
solveInd n p = do
  let ([tl, tr, bl, br], rot) = findPosnsAndRotation n p
  tlBoard <- solve (pred n) tl -- solve defined later, don't worry!
  trBoard <- solve (pred n) tr
  blBoard <- solve (pred n) bl
  brBoard <- solve (pred n) br
  c       <- colorR
  return $ overlay (rot . tromino $ c)
                  $ above (beside tlBoard trBoard)
                          (beside blBoard brBoard)

\end{code}

That's it! our application is now ready. All left is a function that decides whether
we need the base case or the inductive case, and a main.

In main, we also place a circle at the position specified.

\begin{code}
solve :: Int -> Posn -> State StdGen Image
solve 1 = solveBase
solve n = (n `solveInd`)

main :: IO ()
main = do g <- getStdGen
          [nLine, xLine, yLine] <- getArgs
          let n      = read nLine
          let x = read xLine
          let y = read yLine
          let (ans, _) = (`runState` g) . solve n $ (round x, round y)
          drawImage $ placeImage (circle (tileSize / 2) solid black)
                                 ((x * tileSize) + (tileSize / 2)) ((y * tileSize) + (tileSize / 2)) ans
\end{code}
