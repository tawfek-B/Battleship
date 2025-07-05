% Battleship Puzzle Solver AI (v3)
% Inspired by Conceptis logic puzzle video
% Dynamic board size + deduction AI

:- dynamic cell/3.
:- dynamic row_hint/2.
:- dynamic col_hint/2.
:- dynamic size/2.
:- discontiguous place_random_ships/0.

%------------------------
% Configuration
%------------------------

default_board_size(6,6).
default_row_hints([1, 2, 3, 1, 0, 1]).
default_col_hints([0, 2, 2, 2, 1, 1]).

%------------------------
% Initialization
%------------------------

init :-
    clear,
    default_board_size(Rows, Cols),
    assert(size(Rows, Cols)),
    place_random_ships,
    calculate_hints_from_board,
    fill_unknowns.



% Standard battleship fleet for 6x6 board
% 1 ship of size 3, 2 ships of size 2, 3 ships of size 1
standard_fleet([3, 2, 2, 1, 1, 1]).

% Get appropriate fleet for board size
get_fleet_for_board(Rows, Cols, Fleet) :-
    MinDim is min(Rows, Cols),
    (MinDim >= 6 -> 
        standard_fleet(Fleet)
    ; MinDim >= 4 ->
        Fleet = [2, 2, 1, 1]  % Smaller fleet for 4x4 or 5x5
    ; MinDim >= 3 ->
        Fleet = [2, 1, 1]     % Minimal fleet for 3x3
    ; 
        Fleet = [1, 1]        % Very small fleet for tiny boards
    ).

% Place ships with proper sizes
place_random_ships :-
    size(Rows, Cols),
    get_fleet_for_board(Rows, Cols, ShipSizes),
    place_fleet(ShipSizes, Rows, Cols).

% Place a fleet of ships with given sizes
place_fleet([], _, _) :- !.
place_fleet([Size|Sizes], Rows, Cols) :-
    (place_ship_of_size(Size, Rows, Cols) ->
        place_fleet(Sizes, Rows, Cols)
    ;   write('Warning: Skipping ship of size '), write(Size), write(' - could not place.'), nl,
        place_fleet(Sizes, Rows, Cols)  % Continue with remaining ships
    ).

% Place a single ship of given size
place_ship_of_size(Size, Rows, Cols) :-
    place_ship_of_size_with_attempts(Size, Rows, Cols, 100).  % Max 100 attempts

place_ship_of_size_with_attempts(Size, Rows, Cols, Attempts) :-
    Attempts > 0,
    random(0, 2, Orientation),  % 0 = horizontal, 1 = vertical
    (Orientation = 0 ->
        place_horizontal_ship(Size, Rows, Cols)
    ;   place_vertical_ship(Size, Rows, Cols)
    ).
place_ship_of_size_with_attempts(Size, Rows, Cols, Attempts) :-
    Attempts > 0,
    Attempts1 is Attempts - 1,
    place_ship_of_size_with_attempts(Size, Rows, Cols, Attempts1).
place_ship_of_size_with_attempts(_, _, _, 0) :-
    write('Warning: Could not place ship. Board may be too small.'), nl,
    fail.

% Place horizontal ship
place_horizontal_ship(Size, Rows, Cols) :-
    place_horizontal_ship_with_attempts(Size, Rows, Cols, 50).  % Max 50 attempts

place_horizontal_ship_with_attempts(Size, Rows, Cols, Attempts) :-
    Attempts > 0,
    MaxCol is Cols - Size + 1,
    RowsPlus1 is Rows + 1,
    MaxColPlus1 is MaxCol + 1,
    random(1, RowsPlus1, R),
    random(1, MaxColPlus1, C),
    (can_place_horizontal_ship(Size, R, C) ->
        place_horizontal_ship_cells(Size, R, C)
    ;   Attempts1 is Attempts - 1,
        place_horizontal_ship_with_attempts(Size, Rows, Cols, Attempts1)
    ).
place_horizontal_ship_with_attempts(_, _, _, 0) :-
    write('Warning: Could not place horizontal ship.'), nl,
    fail.

% Place vertical ship
place_vertical_ship(Size, Rows, Cols) :-
    place_vertical_ship_with_attempts(Size, Rows, Cols, 50).  % Max 50 attempts

place_vertical_ship_with_attempts(Size, Rows, Cols, Attempts) :-
    Attempts > 0,
    MaxRow is Rows - Size + 1,
    MaxRowPlus1 is MaxRow + 1,
    ColsPlus1 is Cols + 1,
    random(1, MaxRowPlus1, R),
    random(1, ColsPlus1, C),
    (can_place_vertical_ship(Size, R, C) ->
        place_vertical_ship_cells(Size, R, C)
    ;   Attempts1 is Attempts - 1,
        place_vertical_ship_with_attempts(Size, Rows, Cols, Attempts1)
    ).
place_vertical_ship_with_attempts(_, _, _, 0) :-
    write('Warning: Could not place vertical ship.'), nl,
    fail.

% Check if we can place a horizontal ship
can_place_horizontal_ship(Size, R, C) :-
    forall(between(0, Size, I),
        (C1 is C + I,
         can_place_ship(R, C1))
    ).

% Check if we can place a vertical ship
can_place_vertical_ship(Size, R, C) :-
    forall(between(0, Size, I),
        (R1 is R + I,
         can_place_ship(R1, C))
    ).

% Place horizontal ship cells
place_horizontal_ship_cells(0, _, _) :- !.
place_horizontal_ship_cells(Size, R, C) :-
    assert(cell(R, C, ship)),
    Size1 is Size - 1,
    C1 is C + 1,
    place_horizontal_ship_cells(Size1, R, C1).

% Place vertical ship cells
place_vertical_ship_cells(0, _, _) :- !.
place_vertical_ship_cells(Size, R, C) :-
    assert(cell(R, C, ship)),
    Size1 is Size - 1,
    R1 is R + 1,
    place_vertical_ship_cells(Size1, R1, C).

% Check if we can place a ship at given position
can_place_ship(R, C) :-
    \+ cell(R, C, ship),  % Position not already occupied
    \+ has_adjacent_ship(R, C).  % No adjacent ships

% Check if there are any orthogonal ships adjacent to the position (no diagonals)
has_adjacent_ship(R, C) :-
    orthogonal_neighbor(R, C, RN, CN),
    cell(RN, CN, ship).

orthogonal_neighbor(R, C, RN, CN) :-
    ( RN is R - 1, CN is C
    ; RN is R + 1, CN is C
    ; RN is R,     CN is C - 1
    ; RN is R,     CN is C + 1
    ),
    within_bounds(RN, CN).


% Calculate hints based on the randomly placed ships
calculate_hints_from_board :-
    size(Rows, Cols),
    calculate_row_hints(Rows),
    calculate_col_hints(Cols).

calculate_row_hints(0) :- !.
calculate_row_hints(R) :-
    size(_, _),
    findall(C, cell(R, C, ship), Ships),
    length(Ships, Count),
    assert(row_hint(R, Count)),
    R1 is R - 1,
    calculate_row_hints(R1).

calculate_col_hints(0) :- !.
calculate_col_hints(C) :-
    size(_, _),
    findall(R, cell(R, C, ship), Ships),
    length(Ships, Count),
    assert(col_hint(C, Count)),
    C1 is C - 1,
    calculate_col_hints(C1).

add_hints(Functor, Predicate) :-
    call(Functor, List),
    length(List, Length),
    numlist(1, Length, Indexes),
    maplist(assert_hint(Predicate), Indexes, List).

assert_hint(Predicate, Index, Value) :-
    Goal =.. [Predicate, Index, Value],
    assertz(Goal).

len(List, Length) :- length(List, Length).

clear :-
    retractall(cell(_,_,_)),
    retractall(size(_,_)),
    retractall(row_hint(_,_)),
    retractall(col_hint(_,_)).

%------------------------
% Unknown Initialization
%------------------------

fill_unknowns :-
    size(Rows, Cols),
    forall(between(1, Rows, R),
        forall(between(1, Cols, C),
            ( \+ cell(R,C,_)
              -> assert(cell(R,C,unknown))
              ; true
            )
        )
    ).

%------------------------
% Main Solve Loop
%------------------------

run :-
    init,
    display_fleet_info,
    solve.

display_fleet_info :-
    size(Rows, Cols),
    get_fleet_for_board(Rows, Cols, Fleet),
    write('Board size: '), write(Rows), write('x'), write(Cols), nl,
    write('Fleet: '), write(Fleet), nl, nl.

solve :-
    solve_loop,
    (solved -> write('Solved!'), nl ; write('Stuck. Couldn\'t solve.'), nl).

solve_loop :-
    apply_rules_once,
    print_board,
    solve_loop.
solve_loop.  % No more progress

%------------------------
% Apply Deduction Rules
%------------------------

apply_rules_once :-
    findall(cell(R,C,V), cell(R,C,V), Before),
    apply_deduction_rules,
    findall(cell(R,C,V), cell(R,C,V), After),
    (Before \= After -> true ; fail).

apply_deduction_rules :-
    rule_zero_row,
    rule_zero_col,
    rule_full_row,
    rule_full_col.

%------------------------
% Rule: Row Hint is 0 → All water
%------------------------
rule_zero_row :-
    row_hint(R, 0),
    size(_, Cols),
    between(1, Cols, C),
    cell(R,C,unknown),
    retract(cell(R,C,unknown)),
    assert(cell(R,C,water)),
    fail.
rule_zero_row.

%------------------------
% Rule: Column Hint is 0 → All water
%------------------------
rule_zero_col :-
    col_hint(C, 0),
    size(Rows, _),
    between(1, Rows, R),
    cell(R,C,unknown),
    retract(cell(R,C,unknown)),
    assert(cell(R,C,water)),
    fail.
rule_zero_col.

%------------------------
% Rule: Row fully deducible → Mark unknowns as ships
%------------------------
rule_full_row :-
    row_hint(R, Count),
    size(_, Cols),
    findall(C, (between(1, Cols, C), cell(R,C,ship)), Ships),
    length(Ships, S),
    UnknownLeft is Count - S,
    findall(CU, (between(1, Cols, CU), cell(R,CU,unknown)), UCells),
    length(UCells, UnknownLeft),
    forall(member(C, UCells), (
        retract(cell(R,C,unknown)),
        assert(cell(R,C,ship))
    )),
    fail.
rule_full_row.

%------------------------
% Rule: Column fully deducible → Mark unknowns as ships
%------------------------
rule_full_col :-
    col_hint(C, Count),
    size(Rows, _),
    findall(R, (between(1, Rows, R), cell(R,C,ship)), Ships),
    length(Ships, S),
    UnknownLeft is Count - S,
    findall(RU, (between(1, Rows, RU), cell(RU,C,unknown)), UCells),
    length(UCells, UnknownLeft),
    forall(member(R, UCells), (
        retract(cell(R,C,unknown)),
        assert(cell(R,C,ship))
    )),
    fail.
rule_full_col.

%------------------------
% Validation
%------------------------

check_rule_1 :-  % No touching ships
    \+ (cell(R,C,ship), neighbor(R,C,RN,CN), cell(RN,CN,ship)).

neighbor(R,C,RN,CN) :-
    between(-1,1,DR), between(-1,1,DC),
    (DR \= 0 ; DC \= 0),
    RN is R + DR, CN is C + DC,
    within_bounds(RN,CN).

check_rule_2 :-  % Row count matches
    forall(row_hint(R, Count), (
        findall(C, cell(R,C,ship), L),
        length(L, Count)
    )).

check_rule_3 :-  % Column count matches
    forall(col_hint(C, Count), (
        findall(R, cell(R,C,ship), L),
        length(L, Count)
    )).

check_rule_4 :-  % No unknowns left
    \+ cell(_,_,unknown).

solved :-
    check_rule_1,
    check_rule_2,
    check_rule_3,
    check_rule_4.

%------------------------
% Printing
%------------------------

print_board :-
    nl,
    size(Rows, Cols),
    forall(between(1, Rows, R), (
        forall(between(1, Cols, C), (
            cell(R,C,V),
            print_cell(V)
        )),
        row_hint(R, RH),
        write(' | '), write(RH), nl
    )),
    forall(between(1, Cols, C), (
        col_hint(C, CH),
        write(' '), write(CH)
    )), nl, nl.

print_cell(ship) :- write(' S').
print_cell(water) :- write(' ~').
print_cell(unknown) :- write(' ?').

%------------------------
% Utility
%------------------------

within_bounds(R,C) :-
    size(Rows, Cols),
    R >= 1, R =< Rows,
    C >= 1, C =< Cols. 