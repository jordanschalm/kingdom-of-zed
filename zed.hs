{-
CPSC 312 Project 2
Kingdom of Zed

Jordan Schalm   37955135
David Julien
Seerat Sekhon
-}

-- Input and output type format
type Clue = [Integer]
type Solution = [[Integer]]

-- Internal puzzle representation
-- While solving the puzzle, we represent the board as a matrix where each cell
-- contains all possible values for that cell.
type Rating = Integer
type County = [Rating]
type Kingdom = [[County]]
data Clues = Clues {
    n :: Clue,
    e :: Clue,
    s :: Clue,
    w :: Clue
}

-- Removes a possible rating from a county. Counties have no duplicate ratings.
rm_rating :: County -> Rating -> County
rm_rating [] _ = []
rm_rating (h:t) r = if h == r then t else h : (rm_rating t r)

-- Clues in order (N, E, S, W)
zed :: (Clue, Clue, Clue, Clue) -> Solution

zed x = [[]]