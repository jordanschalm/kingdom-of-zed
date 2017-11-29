{-
CPSC 312 Project 2
Kingdom of Zed

Jordan Schalm   37955135
David Julien
Seerat Sekhon   35145135
-}

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import Data.List
import Debug.Trace

-- Internal puzzle representation:
-- While solving the puzzle, we represent the board as a matrix where each cell
-- contains all possible values for that cell. We define some terminology:
--   COUNTY - one cell in the matrix
--   KINGDOM - the NxN matrix of counties
--   LANE - a horizontal or vertical list of counties
{-
          NORTH
        1  2  3  4  
      +-----------+
W   4 |  |  |  |  | 4 E
E   3 |  |  |  |  | 3 A
S   2 |  |  |  |  | 2 S
T   1 |  |  |  |  | 1 T
    y +-----------+
      x 1  2  3  4
          SOUTH
-}

---------------------------------------
-- TYPE & DATA DECLARATION
---------------------------------------
-- A rating is an integer from 1..N
type Rating = Int
-- A county is a set of possible ratings
type County = Set.Set Rating
-- A position is an (x,y) coordinate pair denoting the location of a county
-- within the kingdom grid
type Pos = (Int, Int)
-- A lane is a horizontal or vertical line of counties
type Lane = [County]
-- A kingdom is a mapping from positions to counties. This describes the state
-- of the board at a given time during the solve process.
data Kingdom = Kingdom {
    size :: Int,
    counties :: Map.Map Pos County
}

-- Enumerates the set of possible board sides
data Side = North | East | South | West deriving (Eq, Ord, Show)
sides = [North, East, South, West]
-- A clue is an integer from 1..N
type Clue = Int
-- A clue position is a (side, n) pair denoting a merchant position
data CluePos = CluePos {
    side :: Side,
    index :: Int
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
get_size_from_clues :: Input -> Int
get_size_from_clues (n, _, _, _) = length n

-- Gets the size of the kingdom from a puzzle instance
get_size :: Puzzle -> Int
get_size p = size (kingdom p)

-- Returns the county at the given position

get_county :: Kingdom -> Pos -> County
get_county k (x, y) = Maybe.fromJust (Map.lookup (x, y) m)
    where m = counties k

-- set a county in counties at the given position
set_county :: Map.Map Pos County -> Pos -> Rating -> Map.Map Pos County
set_county counties (x, y) rating = (Map.insert (x, y) (Set.singleton rating) counties )

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

-- set one vertical/horizontal for a kingdom at a given side and index and return the updated kingdom

set_lane :: Map.Map Pos County -> Int -> [Int] -> Side -> Int -> Map.Map Pos County
set_lane c _ [] _ _ = c
set_lane counties size ratings side i  
    | side == North =
        foldl (\ mp (j, val) -> (Map.insert (i, j) (Set.singleton val) mp)) counties (zip (reverse [1..size]) ratings)
    | side == East =
        foldl (\ mp (j, val)  -> (Map.insert (j, i) (Set.singleton val) mp)) counties (zip (reverse [1..size]) ratings)
    | side == South =
        foldl (\ mp (j, val) -> (Map.insert (i, j) (Set.singleton val) mp)) counties (zip [1..size] ratings)
    | side == West =
        foldl (\ mp (j, val) -> (Map.insert (j, i) (Set.singleton val) mp)) counties (zip [1..size] ratings)

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
init_kingdom :: Int -> Kingdom
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

init_clue_side :: [Int] -> Side -> Clues
init_clue_side lst sd
    | sd == North || sd == West =
        _init_clue_side lst sd
    | sd == East || sd == South =
        _init_clue_side (reverse lst) sd

-- Helper function for init_clue_side formats a list, assuming the list is
-- ordered as we need it given the side.
_init_clue_side :: [Int] -> Side -> Clues
_init_clue_side lst sd = Map.fromList [((CluePos sd i), clue) | (i, clue) <- zip [1..] lst]

---------------------------------------
-- DISPLAY 
---------------------------------------
-- Display Kingdom

-- printKingdom :: Kingdom -> [IO ()]
-- printKingdom k =
--     -- [ mapM_ print printLst| printLst <- [foldl (\acc (pos, county) -> 1:acc) [] lst | 
--     --     lst <- (Map.toList k)]]
--     [mapM_ print lst | lst <- Map.toList k]

---------------------------------------
-- VALIDATOR
---------------------------------------

-- constraints: 
-- Each integer 1.. N appears in each horizontal Lane exactly once
-- Each integer 1.. N appears in each vertical Lane exactly once
-- The Clue for each horizontal Lane is satisfied
-- The Clue for each vertical Lane is satisfied

-- extract values 
is_board_valid :: [[Int]] -> Puzzle -> Bool
is_board_valid rows puzzle = 
    (verify (Kingdom s (fill_in_counties k rows s)) (clues puzzle))
    where 
        k = kingdom puzzle
        s = size k

-- check both duplicates and clues
verify :: Kingdom -> Clues -> Bool
verify k clues =
    (verify_unique_lanes clues k) && (verify_clues clues k)


-- take a kingdom and fill in counties with guess ratings
fill_in_counties :: Kingdom -> [[Int]] -> Int -> Map.Map Pos County
fill_in_counties k rows size = 
    snd (foldl (\(index, c) row -> (index - 1, (set_lane c size row West index))) (size, c) rows)
        where 
            c = (counties k)

-- verify whether lanes from north to south are unique 
verify_unique_lanes :: Clues -> Kingdom -> Bool
verify_unique_lanes clues k =
    foldl (\ acc index -> acc && is_unique_lane(get_lane (Puzzle k clues) (CluePos North index))) True [1..s] 
        where s = size k

is_unique_lane :: Lane -> Bool
is_unique_lane [] = True
is_unique_lane (x:xs) = x `notElem` xs && is_unique_lane xs 
-- verify_clues clues k size | trace ("verify_clues " ++ show clues ++ "\n") False = undefined

verify_clues :: Clues -> Kingdom -> Bool
verify_clues clues k =
    foldl (\ acc (cluepos, clue) -> acc && (is_clue_correct (lane_to_list (get_lane (Puzzle k clues) cluepos)) clue))  True (Map.assocs clues) 


-- given lane ratings and the clue for that lane (in correct order as retrieved by get_lane) return true if the clue matches the lane
is_clue_correct :: [Int] -> Clue -> Bool
is_clue_correct [] _ = True
is_clue_correct ratings clue = (calculate_ranking ratings 0) == clue

lane_to_list :: Lane -> [Int]
lane_to_list lane = foldr (\ e acc -> (Set.elems e)++acc) [] lane
-- calculate the ranking for a list of counties
calculate_ranking :: [Int] -> Int -> Int
calculate_ranking [] _ = 0
calculate_ranking (x:xs) highest_so_far
    | x > highest_so_far = 1 + (calculate_ranking xs x)
    | otherwise = (calculate_ranking xs highest_so_far)



---------------------------------------
-- SOLVER
---------------------------------------
-- Define the input/outputs to the solver.
-- Clues in order (N, E, S, W)
type Input = ([Int], [Int], [Int], [Int])
type Output = [[Int]]

-- Brute force solution

all_permutations = permutations [1,2,3,4]

type Choice a = [a]

choose :: [a] -> Choice a
choose xs = xs


return1 :: a -> Choice a
return1 x = choose [x]

-- Define a "zero" for our monad.  This
-- represents failure.
mzero :: Choice a
mzero = choose []

-- Either fail, or return something
-- useless and continue the computation.
guard :: Bool -> Choice ()
guard True  = return ()
guard False = mzero


-- brute force permutation generating
-- g
solve :: Puzzle -> [[[Int]]]
solve puzzle = do
    row1 <- choose all_permutations
    row2 <- choose all_permutations
    row3 <- choose all_permutations
    row4 <- choose all_permutations
    -- check if permutation is valid
    guard(is_board_valid (row1:row2:row3:row4:[]) puzzle)
    -- if it is, return it
    return (row1:row2:row3:row4:[])

-- return solution
get_solution :: Puzzle -> [[[Int]]]
get_solution puzzle = take 1 (solve puzzle)

zed :: Input -> Output
zed clues = (head (get_solution (init_puzzle clues)))

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

