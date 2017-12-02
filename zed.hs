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
import Debug.Trace

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
-- Enumerates the set of possible board sides
data Side = North | East | South | West deriving (Eq, Ord, Show)
-- A county is a set of possible ratings
type County = Set.Set Integer
-- A position is an (x,y) coordinate pair denoting the location of a county
-- within the kingdom grid
type Pos = (Integer, Integer)
-- A clue position is a (side, n) pair denoting a merchant position
type CluePos = (Side, Integer)
-- A clue map maps clue positions to clues
type ClueMap = Map.Map CluePos Integer
-- A lane is a horizontal or vertical line of counties
type Lane = [County]
-- A kingdom is a mapping from positions to counties. This describes the state
-- of the board at a given time during the solve process.
data Kingdom = Kingdom {
    size :: Integer,
    counties :: Map.Map Pos County,
    clues :: ClueMap
}
instance Eq Kingdom where
    (Kingdom size1 counties1 clues1) == (Kingdom size2 counties2 clues2) =
        (size1 == size2) && (counties1 == counties2) && (clues1 == clues2)

-- Define the input/outputs to the solver.
-- Clues in order (N, E, S, W)
type Input = ([Integer], [Integer], [Integer], [Integer])
type Output = [[Integer]]

---------------------------------------
-- GETTERS/SETTERS
---------------------------------------
-- Gets the county at the given position
k_get :: Kingdom -> Pos -> County
k_get k pos = m_lookup pos (counties k)

-- Sets the county at the given position
k_set :: Kingdom -> Pos -> County -> Kingdom
k_set k pos county = k { counties = Map.insert pos county (counties k) }

-- Gets the lane corresponding to the given clue position
k_get_lane :: Kingdom -> CluePos -> Lane
k_get_lane k pos = map (\ p -> k_get k p) (get_lane_coords pos n)
    where n = (size k)

-- Sets the lane corresponding to the given clue position
k_set_lane :: Kingdom -> CluePos -> Lane -> Kingdom
k_set_lane k cpos lane = foldr
    (\ (pos, county) _k -> k_set _k pos county)
    k
    (zip (get_lane_coords cpos n) lane)
    where n = (size k)

-- Gets the clue for the given position
k_get_clue :: Kingdom -> CluePos -> Integer
k_get_clue k pos = m_lookup pos (clues k)

-- Returns some unsolved county position, if one exists
k_get_unsolved :: Kingdom -> Maybe Pos
k_get_unsolved k = _k_get_unsolved k (all_coords (size k))

_k_get_unsolved :: Kingdom -> [Pos] -> Maybe Pos
_k_get_unsolved k [] = Nothing
_k_get_unsolved k (pos:t) = if k_county_solved k pos then _k_get_unsolved k t else Just pos

-- Returns true if the county at the given position is solved
k_county_solved :: Kingdom -> Pos -> Bool
k_county_solved k pos = (len (k_get k pos)) == 1

-- Returns true if the kingdom is in a solved state. Doesn't check correctness,
-- just that each county has one rating. Rules will handle checking correctness.
k_is_solved :: Kingdom -> Bool
k_is_solved k = all
    (\ pos -> k_county_solved k pos)
    (all_coords (size k))

-- Returns a string representation of the kingdom
instance Show Kingdom where show k = k_str k

k_str :: Kingdom -> String
k_str k = foldr (\ row str -> (k_row_str row) ++ "\n" ++ str) "" (k_get_rows k)

k_row_str :: Lane -> String
k_row_str lane = foldr (\ county str -> (k_county_str county) ++ "\t" ++ str) "" lane

k_county_str :: County -> String
k_county_str county = foldr (\ val str -> (show val) ++ str) "" (Set.toList county)

-- Formats a kingdom for output
k_format :: Maybe Kingdom -> [[Integer]]
k_format k
    | k == Nothing = [[]]
    | otherwise = [[Set.elemAt 0 c | c <- row] | row <- k_get_rows (Maybe.fromJust k)]

-- Returns a list of lanes that correspond to the in-order rows of the kingdom map.
k_get_rows k = [k_get_lane k pos | pos <- [(West, i) | i <- reverse [1..n]]]
    where n = size k

---------------------------------------
-- ENTRYPOINT
---------------------------------------
zed :: Input -> Output
zed clues = k_format (solve (Just (apply_heuristics (init_kingdom clues))))

-- Recursive solver. If a solution exists, returns it, otherwise returns Nothing.
solve :: Maybe Kingdom -> Maybe Kingdom
solve k
        | k == Nothing = Nothing
        | k_next == Nothing = Nothing
        | k_is_solved (Maybe.fromJust k_next) = k_next
        | maybe_unsolved_pos == Nothing = Nothing
        | maybe_unsolved_opts == Nothing = Nothing
        | otherwise =  some
            [_solve_unwrap k_next maybe_unsolved_pos (Set.singleton guess) |
            guess <- Set.toList (Maybe.fromJust maybe_unsolved_opts)]
    where 
        k_next = apply_rules k
        maybe_unsolved_pos = _get_unsolved k_next
        maybe_unsolved_opts = _get_unsolved_opts k_next maybe_unsolved_pos

_get_unsolved :: Maybe Kingdom -> Maybe Pos
_get_unsolved k
    | k == Nothing = Nothing
    | otherwise = k_get_unsolved (Maybe.fromJust k)

_get_unsolved_opts :: Maybe Kingdom -> Maybe Pos -> Maybe County
_get_unsolved_opts k pos
    | k == Nothing = Nothing
    | pos == Nothing = Nothing
    | otherwise = Just (k_get (Maybe.fromJust k) (Maybe.fromJust pos))

-- Calls solve, but unwraps a bunch of stuff
_solve_unwrap :: Maybe Kingdom -> Maybe Pos -> County -> Maybe Kingdom
_solve_unwrap k pos county
    | k == Nothing = Nothing
    | pos == Nothing = Nothing
    | otherwise = solve (Just (k_set (Maybe.fromJust k) (Maybe.fromJust pos) county))

---------------------------------------
-- HELPERS
---------------------------------------
-- Like length but returns an Integer
len n = toInteger (length n)

-- Returns the first thing that isn't nothing :)
some [] = Nothing
some (h:t) = if h /= Nothing then h else some t

-- Returns all possible coordinate values for a kingdom of size n
all_coords n = [(x, y) | x <- [1..n], y <- [1..n]]

-- Returns all possible clue coordinate values for a kingdom of size n
all_clue_coords n = [(side, i) | side <- sides, i <- [1..n]]

-- Returns the kingdom size from the input
size_from_input (n, _, _, _) = len n

-- List of sides in order
sides = [North, East, South, West]

-- Looks up an element in a map by key (it's gotta be there though)
m_lookup k m = Maybe.fromJust (Map.lookup k m)

-- Flattens a list of lists into a single list
flatten list = foldr (\ x y -> x ++ y) [] list

-- Counts the instances of x in a list
count x list = len (filter (==x) list)

-- Gets a list of the coordinates of each county in a lane
get_lane_coords :: CluePos -> Integer -> [Pos]
get_lane_coords pos n
    | side == North = [(i, j) | j <- (reverse [1..n])]
    | side == East = [(j, i) | j <- (reverse [1..n])]
    | side == South = [(i, j) | j <- [1..n]]
    | side == West = [(j, i) | j <- [1..n]]
    where
        side = fst pos
        i = snd pos

-- Returns the number of counties a merchant would visit along the given lane.
-- Assumes the lane is complete (each county has only one possible rating)
n_visited :: Lane -> Integer
n_visited lane = _n_visited lit_lane 0 0
    where lit_lane = map (\ c -> Set.elemAt 0 c) lane

-- Helper: Lane -> countAcc -> top -> count
_n_visited :: [Integer] -> Integer -> Integer -> Integer
_n_visited [] count _ = count
_n_visited (h:t) count top
    | h > top = _n_visited t (count + 1) h
    | otherwise = _n_visited t count top

---------------------------------------
-- INITIALIZATION
---------------------------------------
-- Initializes a kingdom with a given size
init_kingdom :: Input -> Kingdom
init_kingdom clues = Kingdom
    n
    (init_counties n)
    (init_clues clues)
    where n = size_from_input clues

-- Initializes a NxN map of counties
init_counties n = foldr
    (\ pos _map -> Map.insert pos (init_county n) _map)
    Map.empty
    (all_coords n)

-- Initializes a county with all possible ratings
init_county n = Set.fromList [1..n]

--Initializes all the clues
init_clues :: Input -> ClueMap
init_clues (n, e, s, w) =
    foldr Map.union Map.empty
        (map (\ (lst, side) -> init_clue_side lst side) (zip [n, e, s, w] sides))

-- Initializes the clues from one side of the kingdom. Because of the way
-- the input is formatted, N/W are in the correct order, E/S are not.
init_clue_side :: [Integer] -> Side -> ClueMap
init_clue_side list side
    | side == North || side == West =
        _init_clue_side list side
    | side == East || side == South =
        _init_clue_side (reverse list) side

-- Helper function for init_clue_side formats a list, assuming the list is
-- ordered as we need it given the side.
_init_clue_side :: [Integer] -> Side -> ClueMap
_init_clue_side list side = Map.fromList [((side, i), clue) | (i, clue) <- zip [1..] list]

---------------------------------------
-- HEURISTICS
-- Heuristic functions apply some simple heuristics to the kingdom to remove
-- possible ratings from counties before we start the general search. We don't
-- check for contradiction here -- it will get caught later.
---------------------------------------
type Heuristic = Kingdom -> CluePos -> Kingdom
-- If the clue=N, the lane must be 1..N
h_ordered :: Heuristic
h_ordered k pos
        | clue == n = k_set_lane k pos [Set.fromList [x] | x <- [1..n]]
        | otherwise = k
    where
        lane = k_get_lane k pos
        clue = k_get_clue k pos
        n = (size k)

-- If the clue=1, the lane starts with N
h_1 :: Heuristic
h_1 k pos
        | clue == 1 = k_set_lane k pos ((Set.fromList [n]):rest)
        | otherwise = k
    where
        (h:rest) = k_get_lane k pos
        clue = k_get_clue k pos
        n = (size k)

-- if the clue=1<K<N, then the first K-1 squares cannot contain N,
-- the first K-2 squares cannot contain N-1, etc.
-- TODO
h_n_after :: Heuristic
h_n_after k pos = k

-- Applies all heuristics and returns the reduced kingdom.
apply_heuristics :: Kingdom -> Kingdom
apply_heuristics k = foldr
        (\ pos _k -> _apply_heuristics _k pos)
        k
        (all_clue_coords (size k))

-- Applies all heuristics for a single clue position
_apply_heuristics :: Kingdom -> CluePos -> Kingdom
_apply_heuristics k pos = (h_ordered (h_1 (h_n_after k pos) pos) pos)

---------------------------------------
-- RULES
-- Rule functions apply some rule to a kingdom and return Nothing if the rule
-- was violated in the input or a just-wrapped kingdom with the rule's
-- constraints applied.
---------------------------------------
type Rule = Kingdom -> CluePos -> Maybe Kingdom

-- A lane must have no empty squares
r_no_empty :: Rule
r_no_empty k pos
    | all (\ c -> (len c) > 0) (k_get_lane k pos) = Just k
    | otherwise = Nothing

-- A lane must contain all possible ratings
r_no_missing :: Rule
r_no_missing k pos
        | len (foldr (\ c set -> Set.union c set) Set.empty lane) == n = Just k
        | otherwise = Nothing
    where
        lane = (k_get_lane k pos)
        n = size k

-- A lane must not contain duplicates. If we've decided on a value (county with
-- one option) we can remove it from all other counties. If we've "decided" on
-- the same value twice, that's a contradiction.
r_unique :: Rule
r_unique k pos
        | (len (Set.fromList det_vals)) /= (len det_vals) = Nothing
        | otherwise = Just (k_set_lane k pos (rm_dupes lane det_vals))
    where
        lane = k_get_lane k pos
        det_vals = get_det_vals lane []

-- Accumulates all determined values for a lane
get_det_vals [] acc = acc
get_det_vals (h:t) acc
    | len h == 1 = get_det_vals t ((Set.elemAt 0 h):acc)
    | otherwise = get_det_vals t acc

-- Removes duplicates from a lane, given a list of determined values.
rm_dupes [] _ = []
rm_dupes (h:t) det_vals
    | (len h) > 1 =
        (foldr
            (\ val set -> Set.delete val set)
            h
            det_vals
        ):(rm_dupes t det_vals)
    | otherwise = h:(rm_dupes t det_vals)

-- If a lane contains a rating only once, set that county to that rating.
r_one_rating :: Rule
r_one_rating k pos = Just (k_set_lane k pos [set_singles c singles | c <- lane])
    where
        lane = k_get_lane k pos
        singles = get_singles lane

-- Gets all ratings that exist only once in a rating
get_singles lane = filter (\ v -> (count v flat_lane) == 1) [1..n]
    where
        n = len lane
        flat_lane = flatten [Set.toList s | s <- lane]

-- If the county contains a single, set the county to that rating
-- We assume the county contains only one single.
set_singles county singles
        | (len single) > 0 = Set.fromList single
        | otherwise = county
    where
        single = filter (\ s -> Set.member s county) singles

-- If a lane is complete, it must satisfy its clue.
r_complete_valid :: Rule
r_complete_valid k pos
        | is_complete = if (n_visited lane) == clue then Just k else Nothing
        | otherwise = Just k
    where
        lane = k_get_lane k pos
        clue = k_get_clue k pos
        is_complete = all (\ c -> (len c) == 1) lane

-- List of all rule functions
rules = [r_complete_valid, r_no_empty, r_no_missing, r_unique, r_one_rating]

-- Applies all rules for a given kingodm state. If a contradiction is reached,
-- return Nothing, otherwise return the reduced kingdom.
apply_rules :: Maybe Kingdom -> Maybe Kingdom
apply_rules k
    | k == Nothing = Nothing
    | otherwise = foldr (\ rule _k -> apply_one_rule _k rule) k rules

-- Applies one rule for all clue positions.
apply_one_rule :: Maybe Kingdom -> Rule -> Maybe Kingdom
apply_one_rule k rule
    | k == Nothing = Nothing
    | otherwise = foldr
        (\ pos _k -> apply_rule_helper _k pos rule)
        k
        (all_clue_coords (size (Maybe.fromJust k)))

apply_rule_helper :: Maybe Kingdom -> CluePos -> Rule -> Maybe Kingdom
apply_rule_helper k pos rule
    | k == Nothing =  Nothing
    | otherwise = rule (Maybe.fromJust k) pos
