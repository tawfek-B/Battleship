% Battleship Puzzle Solver AI (v4.2)
% Fixed version with corrected forall usage

:- dynamic cell/3.
:- dynamic row_hint/2.
:- dynamic col_hint/2.
:- dynamic size/2.
:- dynamic hidden_ship/2.
:- dynamic ships_remaining/1.
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
    initialize_ai_board,
    count_ships_remaining.

clear :-
    retractall(cell(_,_,_)),
    retractall(size(_,_)),
    retractall(row_hint(_,_)),
    retractall(col_hint(_,_)),
    retractall(hidden_ship(_,_)),
    retractall(ships_remaining(_)).

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
    Max is Size - 1,
    forall(between(0, Max, I),
        (C1 is C + I,
         can_place_ship(R, C1))
    ).

% Check if we can place a vertical ship
can_place_vertical_ship(Size, R, C) :-
    Max is Size - 1,
    forall(between(0, Max, I),
        (R1 is R + I,
         can_place_ship(R1, C))
    ).

% Place horizontal ship cells
place_horizontal_ship_cells(0, _, _) :- !.
place_horizontal_ship_cells(Size, R, C) :-
    assert(hidden_ship(R, C)),
    Size1 is Size - 1,
    C1 is C + 1,
    place_horizontal_ship_cells(Size1, R, C1).

% Place vertical ship cells
place_vertical_ship_cells(0, _, _) :- !.
place_vertical_ship_cells(Size, R, C) :-
    assert(hidden_ship(R, C)),
    Size1 is Size - 1,
    R1 is R + 1,
    place_vertical_ship_cells(Size1, R1, C).

% Check if we can place a ship at given position
can_place_ship(R, C) :-
    \+ hidden_ship(R, C),  % Position not already occupied
    \+ has_adjacent_ship(R, C),  % No adjacent ships
    within_bounds(R, C).

% Check if there are any orthogonal ships adjacent to the position (no diagonals)
has_adjacent_ship(R, C) :-
    orthogonal_neighbor(R, C, RN, CN),
    hidden_ship(RN, CN).

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
    calculate_row_hints(1, Rows),
    calculate_col_hints(1, Cols).

calculate_row_hints(R, Rows) :-
    R =< Rows,
    findall(C, hidden_ship(R, C), Ships),
    length(Ships, Count),
    assert(row_hint(R, Count)),
    R1 is R + 1,
    calculate_row_hints(R1, Rows).
calculate_row_hints(R, Rows) :- R > Rows.

calculate_col_hints(C, Cols) :-
    C =< Cols,
    findall(R, hidden_ship(R, C), Ships),
    length(Ships, Count),
    assert(col_hint(C, Count)),
    C1 is C + 1,
    calculate_col_hints(C1, Cols).
calculate_col_hints(C, Cols) :- C > Cols.

% Initialize AI's view of the board (all unknown)
initialize_ai_board :-
    size(Rows, Cols),
    forall(between(1, Rows, R),
        forall(between(1, Cols, C),
            assert(cell(R, C, unknown))
        )
    ).

% Count how many ship cells remain to be discovered
count_ships_remaining :-
    findall(1, hidden_ship(_, _), Ships),
    length(Ships, Total),
    findall(1, (cell(R, C, ship), hidden_ship(R, C)), Found),
    length(Found, F),
    Remaining is Total - F,
    retractall(ships_remaining(_)),
    assert(ships_remaining(Remaining)).

%------------------------
% Main Solve Loop
%------------------------

run :-
    init,
    display_fleet_info,
    print_board,
    solve.

run_steps(N) :-
    init,
    display_fleet_info,
    print_board,
    solve_steps(N).

display_fleet_info :-
    size(Rows, Cols),
    get_fleet_for_board(Rows, Cols, Fleet),
    write('Board size: '), write(Rows), write('x'), write(Cols), nl,
    write('Fleet: '), write(Fleet), nl,
    findall(1, hidden_ship(_, _), Ships),
    length(Ships, Total),
    write('Total ship cells: '), write(Total), nl, nl.

solve :-
    solve_loop.

solve_loop :-
    ships_remaining(Remaining),
    Remaining > 0,
    apply_rules_once,
    print_board,
    solve_loop.
solve_loop :-
    ships_remaining(0),
    write('Solved! All ships found.'), nl.

solve_steps(0) :-
    ships_remaining(Remaining),
    (Remaining =:= 0 ->
        write('All ships found!'), nl
    ;   write('Stopped after specified steps. '),
        write(Remaining), write(' ship cells remaining.'), nl
    ).

solve_steps(N) :-
    N > 0,
    ships_remaining(Remaining),
    Remaining > 0,
    apply_rules_once,
    print_board,
    N1 is N - 1,
    solve_steps(N1).

%------------------------
% Apply Deduction Rules
%------------------------

apply_rules_once :-
    count_ships_remaining,
    rule_zero_row,
    rule_zero_col,
    rule_full_row,
    rule_full_col,
    rule_guess_if_stuck.

% Rule: Guess a random unknown cell if stuck
rule_guess_if_stuck :-
    ships_remaining(Remaining),
    Remaining > 0,
    findall(cell(R,C,unknown), cell(R,C,unknown), UnknownCells),
    UnknownCells \= [],
    random_member(cell(R,C,unknown), UnknownCells),
    (hidden_ship(R, C) ->
        retract(cell(R,C,unknown)),
        assert(cell(R,C,ship))
    ;   retract(cell(R,C,unknown)),
        assert(cell(R,C,water))
    ),
    count_ships_remaining.

% Rule: Row Hint is 0 → All water
rule_zero_row :-
    row_hint(R, 0),
    size(_, Cols),
    between(1, Cols, C),
    cell(R,C,unknown),
    retract(cell(R,C,unknown)),
    assert(cell(R,C,water)),
    fail.
rule_zero_row.

% Rule: Column Hint is 0 → All water
rule_zero_col :-
    col_hint(C, 0),
    size(Rows, _),
    between(1, Rows, R),
    cell(R,C,unknown),
    retract(cell(R,C,unknown)),
    assert(cell(R,C,water)),
    fail.
rule_zero_col.

% Rule: Row fully deducible → Mark unknowns as ships
rule_full_row :-
    row_hint(R, Count),
    size(_, Cols),
    findall(C, (between(1, Cols, C), cell(R,C,ship)), Ships),
    length(Ships, S),
    UnknownLeft is Count - S,
    UnknownLeft > 0,
    findall(CU, (between(1, Cols, CU), cell(R,CU,unknown)), UCells),
    length(UCells, UnknownLeft),
    forall(member(C, UCells), (
        retract(cell(R,C,unknown)),
        assert(cell(R,C,ship))
    )),
    count_ships_remaining,
    fail.
rule_full_row.

% Rule: Column fully deducible → Mark unknowns as ships
rule_full_col :-
    col_hint(C, Count),
    size(Rows, _),
    findall(R, (between(1, Rows, R), cell(R,C,ship)), Ships),
    length(Ships, S),
    UnknownLeft is Count - S,
    UnknownLeft > 0,
    findall(RU, (between(1, Rows, RU), cell(RU,C,unknown)), UCells),
    length(UCells, UnknownLeft),
    forall(member(R, UCells), (
        retract(cell(R,C,unknown)),
        assert(cell(R,C,ship))
    )),
    count_ships_remaining,
    fail.
rule_full_col.

%------------------------
% Printing
%------------------------

print_board :-
    nl,
    size(Rows, Cols),
    write('  '),
    print_column_numbers(1, Cols),
    nl,
    print_rows(1, Rows, Cols),
    write('  '),
    print_column_hints(1, Cols),
    ships_remaining(Remaining),
    write('\nShips parts remaining: '), write(Remaining), nl, nl.

print_column_numbers(C, Cols) :-
    C =< Cols,
    write('   '), write(C),
    C1 is C + 1,
    print_column_numbers(C1, Cols).
print_column_numbers(C, Cols) :- C > Cols.

print_rows(R, Rows, Cols) :-
    R =< Rows,
    write(R), write(' '),
    print_row_cells(R, 1, Cols),
    row_hint(R, RH),
    write(' | '), write(RH), nl,
    R1 is R + 1,
    print_rows(R1, Rows, Cols).
print_rows(R, Rows, _) :- R > Rows.

print_row_cells(R, C, Cols) :-
    C =< Cols,
    cell(R, C, V),
    print_cell(V),
    C1 is C + 1,
    print_row_cells(R, C1, Cols).
print_row_cells(_, C, Cols) :- C > Cols.

print_column_hints(C, Cols) :-
    C =< Cols,
    col_hint(C, CH),
    write('   '), write(CH),
    C1 is C + 1,
    print_column_hints(C1, Cols).
print_column_hints(C, Cols) :- C > Cols.

print_cell(ship) :- write('   S').
print_cell(water) :- write('   ~').
print_cell(unknown) :- write('   ?').

%------------------------
% Utility
%------------------------

within_bounds(R,C) :-
    size(Rows, Cols),
    R >= 1, R =< Rows,
    C >= 1, C =< Cols.

random_member(X, List) :-
    length(List, Len),
    Len > 0,
    random(0, Len, Index),
    nth0(Index, List, X).