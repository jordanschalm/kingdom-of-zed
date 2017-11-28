{-
CPSC 312 Project 2
Kingdom of Zed

Jordan Schalm   37955135
David Julien
Seerat Sekhon
-}

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

-- Internal puzzle representation:
-- While solving the puzzle, we represent the board as a matrix where each cell
-- contains all possible values for that cell. We define some terminology:
--   COUNTY - one cell in the matrix
--   KINGDOM - the NxN matrix of counties
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
-- A position is an (x,y) coordinate pair denoting the location of a county
-- within the kingdom grid
type Pos = (Integer, Integer)
-- A lane is a horizontal or vertical line of counties
type Lane = [County]
-- A kingdom is a mapping from positions to counties. This describes the state
-- of the board at a given time during the solve process.
data Kingdom = Kingdom {
    size :: Integer,
    counties :: Map.Map Pos County
}

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

-- A heuristic function takes a puzzle and a clue position, performs some
-- heuristic constraint checking, and either returns a puzzle with additional
-- constraints imposed (closer to being solved) or Nothing if a contradiction
-- was found.
type HeuristicFn = Puzzle -> CluePos -> Maybe Puzzle

---------------------------------------
-- HELPER FUNCTIONS
---------------------------------------
-- Determines the size of the kingdom from the clues input
get_size_from_clues :: Input -> Integer
get_size_from_clues (n, _, _, _) = toInteger (length n)

-- Gets the size of the kingdom from a puzzle instance
get_size :: Puzzle -> Integer
get_size p = size (kingdom p)

-- Returns the county at the given position
get_county :: Kingdom -> Pos -> County
get_county k (x, y) = Maybe.fromJust (Map.lookup (x, y) m)
    where m = counties k

-- Gets the clue at the given position
get_clue :: Puzzle -> CluePos -> Clue
get_clue puzzle pos = Maybe.fromJust (Map.lookup pos (clues puzzle))

-- Constructs a lane for the given clue position (side, index)
get_lane :: Puzzle -> CluePos -> Lane
get_lane puzzle cp
    | sd == North =
        map (\ j -> get_county k (i, j)) (reverse [1..n])
    | sd == East =
        map (\ j -> get_county k (j, i)) (reverse [1..n])
    | sd == South =
        map (\ j -> get_county k (i, j)) [1..n]
    | sd == West =
        map (\ j -> get_county k (j, i)) [1..n]
    where
        k = kingdom puzzle
        n = size k
        sd = side cp
        i = index cp

-- Removes a possible rating from a county
rm_rating :: County -> Rating -> County
rm_rating county rating = Set.delete rating county

-- Asserts the only possible rating for a county
set_rating :: County -> Rating -> County
set_rating county rating = Set.fromList [rating]

---------------------------------------
-- INITIALIZATION
---------------------------------------
-- Creates a puzzle instance to work with from an input
init_puzzle :: Input -> Puzzle
init_puzzle clues = Puzzle
    (init_kingdom (get_size_from_clues clues))
    (init_clues clues)

init_county n = Set.fromList [1..n]

-- Initializes a kingdom with a given size
init_kingdom :: Integer -> Kingdom
init_kingdom n = Kingdom n
    (foldr (\ pos mp -> Map.insert pos (init_county n) mp) Map.empty
        [(x, y) | x <- [1..n], y <- [1..n]])

--Initializes all the clues
init_clues :: Input -> Clues
init_clues (n, e, s, w) =
    foldr Map.union Map.empty
        (map (\ (lst, sd) -> init_clue_side lst sd) (zip [n, e, s, w] sides))

-- Initializes the clues from one side of the kingdom. Because of the way
-- the input is formatted, N/W are in the correct order, E/S are not.
init_clue_side :: [Integer] -> Side -> Clues
init_clue_side lst sd
    | sd == North || sd == West =
        _init_clue_side lst sd
    | sd == East || sd == South =
        _init_clue_side (reverse lst) sd

-- Helper function for init_clue_side formats a list, assuming the list is
-- ordered as we need it given the side.
_init_clue_side :: [Integer] -> Side -> Clues
_init_clue_side lst sd = Map.fromList [((CluePos sd i), clue) | (i, clue) <- zip [0..] lst]

---------------------------------------
-- SOLVER
---------------------------------------
-- Define the input/outputs to the solver.
-- Clues in order (N, E, S, W)
type Input = ([Integer], [Integer], [Integer], [Integer])
type Output = [[Integer]]
zed :: Input -> Output
-- Placeholder to prevent errors
zed x = [[]]

---------------------------------------
-- CONSTRAINT HEURISTICS
---------------------------------------
-- If a clue is equal to the size of the kingdom, we know exactly how the
-- corresponding lane must look. If we encounter a contradiction, return
-- Nothing.
ordered_ch :: HeuristicFn
ordered_ch puzzle cp
    | clue == n = Just puzzle
    | otherwise = Just puzzle
    where
        lane = get_lane puzzle cp
        clue = get_clue puzzle cp
        n = get_size puzzle