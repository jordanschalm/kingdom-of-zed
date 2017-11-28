{-
CPSC 312 Project 2
Kingdom of Zed

Jordan Schalm   37955135
David Julien
Seerat Sekhon
-}

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Internal puzzle representation:
-- While solving the puzzle, we represent the board as a matrix where each cell
-- contains all possible values for that cell. We define some terminology:
--   COUNTY - one cell in the matrix
--   LANE - a horizontal or vertical list of counties
{-
          NORTH
      +-----------+
W   4 |  |  |  |  |  E
E   3 |  |  |  |  |  A
S   2 |  |  |  |  |  S
T   1 |  |  |  |  |  T
    y +-----------+
      x 1  2  3  4
          SOUTH
-}

---------------------------------------
-- TYPE & DATA DECLARATION
---------------------------------------
-- A rating is an integer from 1..N
type Rating = Integer
-- A county is a set of possible ratings
type County = Set.Set Rating
init_county = Set.fromList [1..4]
-- A position is an (x,y) coordinate pair denoting the location of a county
-- within the kingdom grid
type Pos = (Integer, Integer)
-- A kingdom is a mapping from positions to counties. This describes the board
type Kingdom = Map.Map Pos County

-- Enumerates the set of possible board sides
data Side = North | East | South | West deriving (Eq, Ord, Show)
sides = [North, East, South, West]
-- A clue is an integer from 1..N
type Clue = Integer
-- A clue position is a (side, n) pair denoting a merchant position
data CluePos = CluePos {
    side :: Side,
    index :: Integer
}
instance Eq CluePos where
    (CluePos s1 i1) == (CluePos s2 i2) = s1 == s2 && i1 == i2
instance Ord CluePos where
    (CluePos s1 i1) <= (CluePos s2 i2)
        | s1 /= s2 = s1 <= s2
        | otherwise = i1 <= i2
instance Show CluePos where
    show (CluePos s i) = "{" ++ (show s) ++ " " ++ (show i) ++ "}"
-- Describes the clues provided by all merchants
type Clues = Map.Map CluePos Clue

-- Represents the entire state of a given puzzle
data Puzzle = Puzzle {
    kingdom :: Kingdom,
    clues :: Clues
}

-- Helper functions
enumerate lst = zip [0..] lst

-- Define the input/outputs to the solver.
-- Clues in order (N, E, S, W)
type Input = ([Integer], [Integer], [Integer], [Integer])
type Output = [[Integer]]
zed :: Input -> Output
-- Placeholder to prevent errors
zed x = [[]]

---------------------------------------
-- INITIALIZATION
---------------------------------------
-- Creates a puzzle instance to work with from an input
init_puzzle :: Input -> Puzzle
init_puzzle clues = Puzzle init_kingdom (init_clues clues)

-- Initializes a kingdom
init_kingdom :: Kingdom
init_kingdom = 
    foldr (\ pos mp -> Map.insert pos init_county mp) Map.empty
        [(x, y) | x <- [1..4], y <- [1..4]]

--Initializes all the clues
init_clues :: Input -> Clues
init_clues (n, e, s, w) =
    foldr Map.union Map.empty
        (map (\ (lst, side) -> init_clue_side lst side) (zip [n, e, s, w] sides))

-- Initializes the clues from one side of the kingdom. Because of the way
-- the input is formatted, N/W are in the correct order, E/S are not.
init_clue_side :: [Integer] -> Side -> Clues
init_clue_side lst side
    | side == North || side == West =
        _init_clue_side lst side
    | side == East || side == South =
        _init_clue_side (reverse lst) side

-- Helper function for init_clue_side formats a list, assuming the list is
-- ordered as we need it given the side.
_init_clue_side :: [Integer] -> Side -> Clues
_init_clue_side lst side = Map.fromList [((CluePos side i), clue) | (i, clue) <- enumerate lst]
