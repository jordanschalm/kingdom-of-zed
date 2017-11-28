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

-- A rating is an integer from 1..N
type Rating = Integer
-- A county is a set of possible ratings
type County = Set.Set Rating
-- A position is an (x,y) coordinate pair denoting the location of a county
-- within the kingdom grid
type Pos = (Integer, Integer)
-- A kingdom is a mapping from positions to counties. This describes the board
type Kingdom = Map.Map Pos County

-- Enumerates the set of possible board sides
type Side = North | East | South | West
-- A clue is an integer from 1..N
type Clue = Integer
-- A clue position is a (side, n) pair denoting a merchant position
type CluePos = (Side, Integer)
-- Describes the clues provided by all merchants
type Clues = Map.Map CluePos Clue

-- Clues in order (N, E, S, W)
zed :: ([Integer], [Integer], [Integer], [Integer]) -> [[Integer]]
-- Placeholder to prevent errors
zed x = [[]]
