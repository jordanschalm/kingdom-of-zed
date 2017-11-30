def rng(n):
    # 1->N
    return range(1, n + 1)


def reverse(lst):
    # Like reversed but returns a list instead of an iterator
    return list(reversed(lst))


def some(lst):
    # Returns the first x in lst where x != False, or False
    for x in lst:
        if x:
            return x
    return False


class Kingdom:
    def __init__(self, n=0, clues=None):
        if not n:
            return
        coords = [(i, j) for i in rng(n) for j in rng(n)]
        self.n = n
        self._coords = coords
        self._clues = self.__gen_clues(clues, n)
        self._map = {c: set(rng(n)) for c in coords}

    def __gen_clues(self, clues, l):
        _clues = {}
        zipped = zip(['n', 'e', 's', 'w'],
                     [clues[0], reverse(clues[1]), reverse(clues[2]), clues[3]])

        for i in range(l):
            for (side, lst) in zipped:
                _clues[(side, i + 1)] = lst[i]
        return _clues

    def __str__(self):
        s = ''
        for i in reverse(rng(self.n)):
            s += str(self.get_lane(('w', i))) + '\n'
        return s

    def score(self):
        return sum([len(self.get(coord)) for coord in self._coords])

    def copy(self):
        k = Kingdom()
        k.n = self.n
        k._coords = self._coords
        k._clues = self._clues
        k._map = self._map.copy()
        return k

    def get_clue(self, clue_coord):
        return self._clues[clue_coord]

    def get_clue_coords(self):
        return [(side, i) for side in ['n', 'e', 's', 'w'] for i in rng(self.n)]

    def get_lane_coords(self, clue_coord):
        side = clue_coord[0]
        index = clue_coord[1]

        if side == 'n':
            return [(index, j) for j in reverse(rng(self.n))]
        if side == 'e':
            return [(j, index) for j in reverse(rng(self.n))]
        if side == 's':
            return [(index, j) for j in rng(self.n)]
        if side == 'w':
            return [(j, index) for j in rng(self.n)]

    def get_lane(self, clue_coord):
        return [self.get(coords) for coords in self.get_lane_coords(clue_coord)]

    def set_lane(self, clue_coord, lane):
        lane_coords = self.get_lane_coords(clue_coord)

        for (coord, square) in zip(lane_coords, lane):
            self.set(coord, square)
        return self

    def square_is_solved(self, coord):
        return len(self.get(coord)) == 1

    def is_solved(self):
        return all([self.square_is_solved(coord) for coord in self._coords])

    def get_unsolved_coord(self):
        for coord in self._coords:
            if not self.square_is_solved(coord):
                return coord

    def is_possible(self):
        return all([len(self.get(coord)) > 0 for coord in self._coords])

    def get(self, coord):
        return self._map[coord].copy()

    def set(self, coord, val):
        if type(val) == int:
            val = [val]
        self._map[coord] = set(val)
        return self


"""
Helper functions
"""


def n_visited(lane):
    """
    Returns how many squares a merchant would visit for the given lane.
    In this case, the lane must contain literal values, not sets of
    possible values.
    """
    count = 0
    top = 0
    for sq in lane:
        if sq > top:
            top = sq
            count += 1
    return count


"""
Heuristic functions apply some heuristic to a kingdom, MODIFYING
the input. They return False if a contradiction was found (ie no
solution exists).

Rule functions do the same, but they can be used multiple times.
"""


def heur_ordered(kingdom, clue_coord):
    """
    if the clue=N, the lane is ordered 1->N
    """
    clue = kingdom.get_clue(clue_coord)
    if clue != kingdom.n:
        return True

    lane = kingdom.get_lane(clue_coord)
    exp_vals = rng(kingdom.n)
    # Contradiction
    if not all([exp in square for (square, exp) in zip(lane, exp_vals)]):
        return False
    kingdom.set_lane(clue_coord, exp_vals)
    return True


def heur_1(kingdom, clue_coord):
    """
    If the clue=1, the lane starts with N
    """
    clue = kingdom.get_clue(clue_coord)
    if clue > 1:
        return True

    lane = kingdom.get_lane(clue_coord)
    # Contradiction
    if not kingdom.n in lane[0]:
        return False
    lane[0] = set([kingdom.n])
    kingdom.set_lane(clue_coord, lane)
    return True


def heur_n_after(kingdom, clue_coord):
    """
    if the clue=1<K<N, then the first K-1 squares cannot contain N,
    the first K-2 squares cannot contain N-1, etc.
    Example: clue=4
    N notin first 3
    N-1 notin first 2
    N-2 notin first 1
    """
    n = kingdom.n
    clue = kingdom.get_clue(clue_coord)
    if clue == 1 or clue == n:
        return True

    lane = kingdom.get_lane(clue_coord)
    k = clue
    j = 1
    while k - j > 0:
        too_big = n - j + 1
        for i in range(k - j):
            if too_big in lane[i]:
                lane[i].remove(too_big)
        j += 1
    kingdom.set_lane(clue_coord, lane)
    return True


def apply_heuristics(kingdom):
    """
    Apply all heuristic functions, return false if a contridictino occurs
    """
    for cc in kingdom.get_clue_coords():
        if not all([
                heur_ordered(kingdom, cc),
                heur_1(kingdom, cc),
                heur_n_after(kingdom, cc)]):
            return False
    return True


def rule_no_empty(kingdom, clue_coord):
    """
    A lane must have no empty squares at any point -- that means we
    elimated all possible options for the square and no solution exists.
    """
    lane = kingdom.get_lane(clue_coord)

    if not all([len(sq) > 0 for sq in lane]):
        print('CONTRADICTION: empty square for coord', clue_coord)
        print('lane', lane)
        return False
    return True


def rule_no_missing(kingdom, clue_coord):
    """
    A lane must contain all possible ratings at least once, otherwise
    no solution exists.
    """
    lane = kingdom.get_lane(clue_coord)
    all_vals = reduce(lambda a, b: a | b, lane, set())
    if len(all_vals) != kingdom.n:
        print('CONTRADICTION: missing value in lane')
        return False
    return True


def rule_unique(kingdom, clue_coord):
    """
    A lane must not contain duplicates
    """
    lane = kingdom.get_lane(clue_coord)
    det_vals = []  # Values we have decided on
    for square in lane:
        if len(square) == 1:
            det_vals.append(next(iter(square)))

    # Contradiction (2 decided values are the same)
    if len(set(det_vals)) != len(det_vals):
        print('CONTRADICTION: 2 same values')
        return False

    # Remove values we've already decided on from other squares
    for square in lane:
        if len(square) > 1:
            for dv in det_vals:
                if dv in square:
                    square.remove(dv)
        if len(square) == 0:
            print('YIKES')
            print('lane/cc: ', kingdom.get_lane(clue_coord), clue_coord)

    kingdom.set_lane(clue_coord, lane)
    return True


def rule_one_option(kingdom, clue_coord):
    """
    If a lane contains one of the rating options only once, we can set
    that square to that option -- there is no other possibility.
    """
    lane = kingdom.get_lane(clue_coord)
    val_counts = {val: 0 for val in rng(kingdom.n)}
    for square in lane:
        for val in square:
            val_counts[val] += 1

    for i in range(len(lane)):
        if len(lane[i]) > 1:
            for val in lane[i]:
                if val_counts[val] == 1:
                    lane[i] = set([val])
                    break

    kingdom.set_lane(clue_coord, lane)
    return True


def rule_complete_valid(kingdom, clue_coord):
    """
    If a lane is complete, verify it satisfies the clue.
    """
    lane = kingdom.get_lane(clue_coord)
    clue = kingdom.get_clue(clue_coord)

    if not all([len(sq) == 1 for sq in lane]):
        return True
    lit_lane = [sq.pop() for sq in lane]
    if n_visited(lit_lane) != clue:
        print('CONTRADICTION: invalid lane', lit_lane, clue_coord)
        print('rule_complete_valid clue/coord: ', clue, clue_coord)
        print('lane: ', lit_lane)
        print('result: ', n_visited(lit_lane))
        print(' ')
    return n_visited(lit_lane) == clue


def apply_rules(kingdom):
    for cc in kingdom.get_clue_coords():
        rule_no_empty(kingdom, cc)
    for cc in kingdom.get_clue_coords():
        rule_no_missing(kingdom, cc)
    for cc in kingdom.get_clue_coords():
        rule_one_option(kingdom, cc)
    print('one-option')
    print(kingdom)
    for cc in kingdom.get_clue_coords():
        rule_unique(kingdom, cc)
    print('unique-lane')
    print(kingdom)
    for cc in kingdom.get_clue_coords():
        rule_complete_valid(kingdom, cc)
    print('complete-valid')
    print(kingdom)
    for cc in kingdom.get_clue_coords():
        if not all([
                rule_no_empty(kingdom, cc),
                rule_no_missing(kingdom, cc),
                rule_one_option(kingdom, cc),
                rule_unique(kingdom, cc),
                rule_complete_valid(kingdom, cc)]):
            return False
    return True


def solve(clues):
    k = Kingdom(len(clues[0]), clues)

    # Apply initial heuristics
    if not apply_heuristics(k):
        return False

    # Recursively search the space until we find a contradiction or a solution
    return _solve(k)


def _solve(k):
    if k is False:
        return False
    print(k.score())
    print(k)
    if not apply_rules(k):
        print('rules failed, returning false')
        return False
    if k.is_solved():
        return k
    # Choose the first square with possibilities
    coord = k.get_unsolved_coord()
    print('Not solved, searching with ', coord)
    square = k.get(coord)
    return some([_solve(k.copy().set(coord, val)) for val in square])
