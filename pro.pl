:- dynamic cell/3.
:- dynamic row_hint/2.
:- dynamic col_hint/2.
:- dynamic size/2.
:- dynamic hidden_ship/2.
:- dynamic ships_remaining/1.
:- dynamic ship_orientation/4.
:- dynamic ship_id/3.
:- dynamic next_ship_id/1.

:- use_module(library(random)).


default_board_size(8,8).


% Tawfek: dynamic, has_adjacent_ship, user_input
% Anas: print_board, print_hints, consult write
% Johnny: rule_zero_col, rule_zero_row, rule_guess_if_stuck, rule_surround_ships_with_water
% Khaled T: hints, count_ships_remaining, user_input
% Khaled J: place_random_ships, solve, solve_steps, rule_surround_ships_with_water
% Haidar: place_random_ships, rule_full_col, rule_full_row


init :-
    clear,
    default_board_size(Rows, Cols),
    assert(size(Rows, Cols)),
    place_random_ships,
    calculate_hints_from_board,
    initialize_board,
    count_ships_remaining.

% just a method to clear everything saved in the memory, called when the
% proccess is finished to re-play the game properly.
clear :-
    retractall(cell(_,_,_)),
    retractall(size(_,_)),
    retractall(row_hint(_,_)),
    retractall(col_hint(_,_)),
    retractall(hidden_ship(_,_)),
    retractall(ships_remaining(_)),
    retractall(ship_orientation(_,_,_,_)),
    retractall(ship_id(_,_,_)),
    retractall(next_ship_id(_)),
    assert(next_ship_id(1)).

% Clear the board and all ship state for manual setup
clear_board_and_state :-
    retractall(cell(_,_,_)),
    retractall(hidden_ship(_,_)),
    retractall(ship_orientation(_,_,_,_)),
    retractall(ship_id(_,_,_)),
    retractall(next_ship_id(_)),
    assert(next_ship_id(1)).


% This is an editable standard battleship fleet for 6x6 board
% We chose 1 ship of size 3, 2 ships of size 2, 3 ships of size 1.
standard_fleet([4, 3, 3, 2, 2, 2, 1, 1, 1, 1]).
% standard_fleet([3, 2, 2, 1, 1, 1]).


% This is a method to get the appropriate fleet for board size.
% the minimum board size set is one, and anything above is an another
% case of fleet board size, each is given an apropriate fleet for the
% borad size set based on the minimum number of rows and columns.
get_fleet_for_board(Rows, Cols, Fleet) :-
    MinDim is min(Rows, Cols),
    (MinDim >= 6 ->
        standard_fleet(Fleet)
    ; MinDim >= 4 ->
        Fleet = [2, 2, 1, 1]
    ; MinDim >= 3 ->
        Fleet = [2, 1, 1]
    ;
        Fleet = [1, 1]
    ).

% A function to place ships with proper sizes in the board.
% it works by repeating the proccess randomly for each ship and sees if
% it is placed correctly one step after another by recognizing that all
% ships given has to be placed in the board correctly matchoing the
% hints.
place_random_ships :-
    size(Rows, Cols),
    get_fleet_for_board(Rows, Cols, ShipSizes),
    place_fleet(ShipSizes, Rows, Cols).


% this is the way we used to place the ships fleet with given sizes of
% each ship in order to place them the right way on the board based on
% the row hints and column hints numbers.
place_fleet([], _, _) :- !.
place_fleet([Size|Sizes], Rows, Cols) :-
    (place_ship_of_size(Size, Rows, Cols) ->
        place_fleet(Sizes, Rows, Cols)
    ;   write('Warning: Skipping ship of size '), write(Size), write(' - could not place.'), nl,
        place_fleet(Sizes, Rows, Cols)  % Continue with remaining ships
    ).



% The previous methods call this fuction that is made to place a
% single ship of given size, after many calls, all ships must be placed
% right in the supposed board square.
place_ship_of_size(Size, Rows, Cols) :-
    place_ship_of_size_with_attempts(Size, Rows, Cols, 100).
    % we sat the max attempts up to 100.


% this is mainly made to try multiple attempts for placing each ship
% correctly, because they need to match the hints and if the method
% started with the wrong ship the solution will be wrong and the
% proccess fails, so this is thw way to make sure every step taken is
% the right one or it''ll go back and try another ship instead.
place_ship_of_size_with_attempts(Size, Rows, Cols, Attempts) :-
    Attempts > 0,
    random(0, 2, Orientation),
    % 0 = horizontal, 1 = vertical
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



% after cosidering if the ship chosen must be sat horizontaly or
% vertically we need a method for each case to actually place
% the horizontal/vertical ship.
% works the same like the ship that takes one square, the same
% methods used but with some spicific editions for each case, but this
% is for the ones that are big and contain many ship parts (2,3,..).
place_horizontal_ship(Size, Rows, Cols) :-
    place_horizontal_ship_with_attempts(Size, Rows, Cols, 50).
    % max attemps sat up to 50 attempts

place_horizontal_ship_with_attempts(Size, Rows, Cols, Attempts) :-
    Attempts > 0,
    MaxCol is Cols - Size + 1,
    RowsPlus1 is Rows + 1,
    MaxColPlus1 is MaxCol + 1,
    random(1, RowsPlus1, R),
    random(1, MaxColPlus1, C),
    (can_place_horizontal_ship(Size, R, C) ->
        place_horizontal_ship_cells(Size, R, C, Size),
        % Increment ship ID for next ship
        retract(next_ship_id(CurrentID)),
        NextID is CurrentID + 1,
        assert(next_ship_id(NextID))
    ;   Attempts1 is Attempts - 1,
        place_horizontal_ship_with_attempts(Size, Rows, Cols, Attempts1)
    ).
place_horizontal_ship_with_attempts(_, _, _, 0) :-
    write('Warning: Could not place horizontal ship.'), nl,
    fail.



% those are the ones for placing the vertical ship, same as previous
% ones for the horizontal.
place_vertical_ship(Size, Rows, Cols) :-
    place_vertical_ship_with_attempts(Size, Rows, Cols, 50).

place_vertical_ship_with_attempts(Size, Rows, Cols, Attempts) :-
    Attempts > 0,
    MaxRow is Rows - Size + 1,
    MaxRowPlus1 is MaxRow + 1,
    ColsPlus1 is Cols + 1,
    random(1, MaxRowPlus1, R),
    random(1, ColsPlus1, C),
    (can_place_vertical_ship(Size, R, C) ->
        place_vertical_ship_cells(Size, R, C, Size),
        % Increment ship ID for next ship
        retract(next_ship_id(CurrentID)),
        NextID is CurrentID + 1,
        assert(next_ship_id(NextID))
    ;   Attempts1 is Attempts - 1,
        place_vertical_ship_with_attempts(Size, Rows, Cols, Attempts1)
    ).
place_vertical_ship_with_attempts(_, _, _, 0) :-
    write('Warning: Could not place vertical ship.'), nl,
    fail.



% A checker to see if we can place a horizontal ship, given the size and
% the placement.
can_place_horizontal_ship(Size, R, C) :-
    Max is Size - 1,
    forall(between(0, Max, I),
        (C1 is C + I,
         can_place_ship(R, C1))
    ).

% same as the previous one but this is for a vertical ship.
can_place_vertical_ship(Size, R, C) :-
    Max is Size - 1,
    forall(between(0, Max, I),
        (R1 is R + I,
         can_place_ship(R1, C))
    ).

% after checking if the ship can be placed or not, if it can then we
% need to place the horizontal ship cells/parts one by one until the
% whole ship gets placed correctly in the right place in the field.
place_horizontal_ship_cells(0, _, _, _) :- !.
place_horizontal_ship_cells(Size, R, C, TotalSize) :-
    assert(hidden_ship(R, C)),
    % store orientation: horizontal ship
    Position is TotalSize - Size + 1,
    assert(ship_orientation(R, C, horizontal, Position)),
    % Assign ship ID to track ship boundaries
    next_ship_id(ShipID),
    assert(ship_id(R, C, ShipID)),
    Size1 is Size - 1,
    C1 is C + 1,
    place_horizontal_ship_cells(Size1, R, C1, TotalSize).

% just like the one before but for placing the vertical ship cells.
place_vertical_ship_cells(0, _, _, _) :- !.
place_vertical_ship_cells(Size, R, C, TotalSize) :-
    assert(hidden_ship(R, C)),
    % store orientation: vertical ship
    Position is TotalSize - Size + 1,
    assert(ship_orientation(R, C, vertical, Position)),
    % Assign ship ID to track ship boundaries
    next_ship_id(ShipID),
    assert(ship_id(R, C, ShipID)),
    Size1 is Size - 1,
    R1 is R + 1,
    place_vertical_ship_cells(Size1, R1, C, TotalSize).

% this function is to make sure if there are any ships next to the position 
adjacent_neighbor(R, C, RN, CN) :-
    between(-1, 1, DR),
    between(-1, 1, DC),
    (DR \= 0 ; DC \= 0), % without the cell itself
    RN is R + DR,
    CN is C + DC,
    within_bounds(RN, CN).

% now we have to check if we can place a ship at given position.
can_place_ship(R, C) :-
    \+ hidden_ship(R, C),
    % case 1: Position not already occupied
    \+ has_adjacent_ship(R, C),
    % case 2: No adjacent ships
    within_bounds(R, C).

%checker to make sure that there are no ships next to or inside each other

has_adjacent_ship(R, C) :-
    adjacent_neighbor(R, C, RN, CN),
    hidden_ship(RN, CN).


% as the ships are being placed, we need to calculate the hints based on
% the randomly placed ships, to make sure no ships are placed outside
% the right place, like every row and column has exactly the same number
% of ship parts as the hint number of that row/column says.
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



% An initializer of the board for the exact given size.
initialize_board :-
    size(Rows, Cols),
    forall(between(1, Rows, R),
        forall(between(1, Cols, C),
            assert(cell(R, C, unknown))
        )
    ).

% As the proccess is going we need to count how many ship cells remain
% to be discovered and then drawn in the right placement in the
% sea/field.
count_ships_remaining :-
    findall(1, hidden_ship(_, _), Ships),
    length(Ships, Total),
    findall(1, (cell(R, C, ship(_,_,_)), hidden_ship(R, C)), Found),
    length(Found, F),
    Remaining is Total - F,
    retractall(ships_remaining(_)),
    assert(ships_remaining(Remaining)).


% now comes the solving functions, when hitting run the board will be
% printed and the solution finding procfcess starts.
% as well as the fleet info printed on the sceen.
run :-
    init,
    display_fleet_info,
    print_grid,
    solve.
    
run_now :-
    display_fleet_info,
    print_grid,
    solve.

% this function is just like the one before, but it varies in the
% solving proccess making it find the solution while printing the board
% after each step instead of prinitng the boead just once when the
% soltution is found.
run_steps(N) :-
    display_fleet_info,
    print_grid,
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
% at first we check the remaining ships to know how many of them need to
% be found, if there is/are ship/s to be found then we continue the
% solving proccess, if ther is not then just end it.
solve_loop :-
    ships_remaining(Remaining),
    (Remaining =:= 0 ->
    fill_unknowns_with_water,
    print_grid,
    write('Solved! All ships found.'), nl;
    apply_rules_once,
    print_grid,
    solve_loop
    ).
    


% just the same as the ones before but made to be called after each step
% made diuring the proccess.
solve_steps(0) :-
    ships_remaining(Remaining),
    (Remaining =:= 0 ->
    fill_unknowns_with_water,
    print_grid,
        write('All ships found!'), nl
    ;   write('Stopped after specified steps. '),
        write(Remaining), write(' ship cells remaining.'), nl
    ).

solve_steps(N) :-
    N > 0,
    ships_remaining(Remaining),
    (Remaining =:= 0 ->
    fill_unknowns_with_water,
    print_grid,
        write('All ships found!'), nl
    ;   apply_rules_once,
        print_grid,
        N1 is N - 1,
        solve_steps(N1)
    ).

% we have put some rules in case there's something went wrong during the
% solving proccess, like having zero number or rows somehow or if the
% function is stuck at some step and doesn't know if a ship has to be
% placed in some possition.
apply_rules_once :-
    count_ships_remaining,
    rule_zero_row,
    rule_zero_col,
    rule_full_row,
    rule_full_col,
    rule_guess_if_stuck.

% rule 1: Guess a random unknown cell if stuck.
% just to make sure the proccess won't stop and crash at some point, so
% if that happens let it choose a random cell and chek if it works or
% not, if not then it'll repeat until it founds something right.
rule_guess_if_stuck :-
    ships_remaining(Remaining),
    Remaining > 0,
    findall(cell(R,C,unknown), cell(R,C,unknown), UnknownCells),
    UnknownCells \= [],
    random_member(cell(R,C,unknown), UnknownCells),
    (hidden_ship(R, C) ->
        retract(cell(R,C,unknown)),
        ship_orientation(R, C, Orientation, Position),
        ship_id(R, C, ShipID),
        assert(cell(R,C,ship(Orientation, Position, ShipID)))
    ;   retract(cell(R,C,unknown)),
        assert(cell(R,C,water))
    ),
    count_ships_remaining.

% Rule 2: Row Hint is 0 then All water all around this row.
rule_zero_row :-
    row_hint(R, 0),
    size(_, Cols),
    between(1, Cols, C),
    cell(R,C,unknown),
    retract(cell(R,C,unknown)),
    assert(cell(R,C,water)),
    fail.
rule_zero_row.

% Rule3: like the one before but only for columns.
rule_zero_col :-
    col_hint(C, 0),
    size(Rows, _),
    between(1, Rows, R),
    cell(R,C,unknown),
    retract(cell(R,C,unknown)),
    assert(cell(R,C,water)),
    fail.
rule_zero_col.

% Rule 4: Row fully deducible, then Mark unknown cells as ships.
rule_full_row :-
    row_hint(R, Count),
    size(_, Cols),
    findall(C, (between(1, Cols, C), cell(R,C,ship(_,_,_))), Ships),
    length(Ships, S),
    UnknownLeft is Count - S,
    UnknownLeft > 0,
    findall(CU, (between(1, Cols, CU), cell(R,CU,unknown)), UCells),
    length(UCells, UnknownLeft),
    forall(member(C, UCells), (
        retract(cell(R,C,unknown)),
        ship_orientation(R, C, Orientation, Position),
        ship_id(R, C, ShipID),
        assert(cell(R,C,ship(Orientation, Position, ShipID)))
    )),
    count_ships_remaining,
    fail.
rule_full_row.

% Rule 5: just like the previous for columns now.
rule_full_col :-
    col_hint(C, Count),
    size(Rows, _),
    findall(R, (between(1, Rows, R), cell(R,C,ship(_,_,_))), Ships),
    length(Ships, S),
    UnknownLeft is Count - S,
    UnknownLeft > 0,
    findall(RU, (between(1, Rows, RU), cell(RU,C,unknown)), UCells),
    length(UCells, UnknownLeft),
    forall(member(R, UCells), (
        retract(cell(R,C,unknown)),
        ship_orientation(R, C, Orientation, Position),
        ship_id(R, C, ShipID),
        assert(cell(R,C,ship(Orientation, Position, ShipID)))
    )),
    count_ships_remaining,
    fail.
rule_full_col.

% Rule 6: sorround a ship with ~ when fully discovered.
% Surround all fully discovered ships with water, but only repeat if a change was made
rule_surround_ships_with_water :-
    setof(ShipID, is_fully_discovered_ship(ShipID), ShipIDs),
    surround_ships_with_water(ShipIDs, Changed),
    Changed = true, !, % Only fail (and thus repeat) if a change was made
    rule_surround_ships_with_water.
rule_surround_ships_with_water.

surround_ships_with_water([], false).
surround_ships_with_water([ShipID|Rest], Changed) :-
    surround_ship_with_water(ShipID, Changed1),
    surround_ships_with_water(Rest, Changed2),
    (Changed1 = true ; Changed2 = true -> Changed = true ; Changed = false).

surround_ship_with_water(ShipID, Changed) :-
    findall((R,C), ship_id(R, C, ShipID), ShipCells),
    surround_cells_with_water(ShipCells, Changed).

surround_cells_with_water([], false).
surround_cells_with_water([(R,C)|Rest], Changed) :-
    surround_cell_with_water(R, C, Changed1),
    surround_cells_with_water(Rest, Changed2),
    (Changed1 = true ; Changed2 = true -> Changed = true ; Changed = false).

surround_cell_with_water(R, C, Changed) :-
    findall((RN,CN), (adjacent_neighbor(R, C, RN, CN), cell(RN, CN, unknown)), Unknowns),
    (Unknowns = [] -> Changed = false ;
        (forall(member((RN,CN), Unknowns), (retract(cell(RN, CN, unknown)), assert(cell(RN, CN, water)))), Changed = true)
    ).

is_fully_discovered_ship(ShipID) :-
    ship_id(R, C, ShipID),
    \+ (ship_id(R2, C2, ShipID), cell(R2, C2, unknown)),
    % Ensure at least one cell is present for this ShipID
    cell(R, C, ship(_,_,ShipID)).

surround_ship_with_water(ShipID) :-
    findall((R,C), ship_id(R, C, ShipID), ShipCells),
    forall(member((R,C), ShipCells), surround_cell_with_water(R, C)).

surround_cell_with_water(R, C) :-
    adjacent_neighbor(R, C, RN, CN),
    cell(RN, CN, unknown),
    retract(cell(RN, CN, unknown)),
    assert(cell(RN, CN, water)),
    fail.
surround_cell_with_water(_, _).

print_ship_arrow_compact(horizontal, 1, ShipID) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    (ShipSize = 1 -> write(' o ') ; write(' < ')).
print_ship_arrow_compact(horizontal, Position, ShipID) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    Position = ShipSize, write(' > ').
print_ship_arrow_compact(horizontal, _, _) :- write(' - ').
print_ship_arrow_compact(vertical, 1, ShipID) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    (ShipSize = 1 -> write(' o ') ; write(' ^ ')).
print_ship_arrow_compact(vertical, Position, ShipID) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    Position = ShipSize, write(' v ').
print_ship_arrow_compact(vertical, _, _) :- write(' | ').


print_cell_symbol(ship(horizontal, Position, ShipID)) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    (ShipSize = 1 -> write(' o ')
    ; Position = 1 -> write(' < ')
    ; Position = ShipSize -> write(' > ')
    ; write('-')).
print_cell_symbol(ship(vertical, Position, ShipID)) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    (ShipSize = 1 -> write(' o ')
    ; Position = 1 -> write(' ^ ')
    ; Position = ShipSize -> write(' v ')
    ; write(' | ')).
print_cell_symbol(_) :- write(' ? ').

print_grid :-
    nl,
    size(Rows, Cols),
    print_column_numbers_grid(Cols),
    print_horizontal_grid_line(Cols),
    print_rows_grid(1, Rows, Cols),
    print_column_hints_grid(Cols),
    ships_remaining(Remaining),
    write('\nShip parts remaining: '), write(Remaining), nl, nl.

print_column_numbers_grid(Cols) :-
    write('    '),
    forall(between(1, Cols, C), format(' ~|~t~d~2+ ', [C])),
    nl.

print_column_hints_grid(Cols) :-
    write('    '),
    forall(between(1, Cols, C), (
        col_hint(C, CH),
        format(' ~|~t~d~2+ ', [CH])
    )),
    nl.
print_horizontal_grid_line(Cols) :-
    write('   +'),
    forall(between(1, Cols, _), write('----')),
    write('+'), nl.

print_rows_grid(R, Rows, Cols) :-
    R =< Rows,
    format('~|~t~d~2+ |', [R]),
    print_row_cells_grid(R, 1, Cols),
    row_hint(R, RH),
    format('| ~|~t~d~2+~n', [RH]),
    print_horizontal_grid_line(Cols),
    R1 is R + 1,
    print_rows_grid(R1, Rows, Cols).
print_rows_grid(R, Rows, _) :- R > Rows.

print_row_cells_grid(_, C, Cols) :-
    C > Cols, !.
print_row_cells_grid(R, C, Cols) :-
    cell(R, C, V),
    print_cell_grid(V),
    C1 is C + 1,
    print_row_cells_grid(R, C1, Cols).

print_cell_grid(ship(Orientation, Position, ShipID)) :-
    print_ship_arrow_grid(Orientation, Position, ShipID).
print_cell_grid(water) :- write('  ~ ').
print_cell_grid(unknown) :- write('  ? ').

print_ship_arrow_grid(horizontal, 1, ShipID) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    (ShipSize = 1 -> write('  o ') ; write('  < ')).
print_ship_arrow_grid(horizontal, Position, ShipID) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    Position = ShipSize, write('  > ').
print_ship_arrow_grid(horizontal, _, _) :- write('  - ').
print_ship_arrow_grid(vertical, 1, ShipID) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    (ShipSize = 1 -> write('  o ') ; write('  ^ ')).
print_ship_arrow_grid(vertical, Position, ShipID) :-
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    Position = ShipSize, write('  v ').
print_ship_arrow_grid(vertical, _, _) :- write('  | ').

within_bounds(R,C) :-
    size(Rows, Cols),
    R >= 1, R =< Rows,
    C >= 1, C =< Cols.

% print instructions when file is consulted
:- write('=== Battleship Solver ==='), nl,
   write('Available commands:'), nl,
   write('  run.           - Solve after initializing board'), nl,
   write('  run_now.           - Solve without init'), nl,
   write('  run_steps(N).  - Solve with N steps, without initalizing'), nl,
   write('  init.          - Initialize a new boad'), nl,
   write('  print_grid.   - Print the current board'), nl,
   write('  user_place_ship/4.   - Places a ship at row R and column C that has size "Size" and is either horizontal/vertical/single'), nl,
   write('  set_water/2.   - Fills a cell with water ~'), nl,
   write('  fill_all_water.   - Fills every cell (That\'s not a ship) with ~'), nl,
   write('  fill_all_unknown.   - Fills every cell (That\'s not a ship) with ?'), nl,
   write('  check_user_solution.   - Checks that no two ships touch each other and every ship has water all the way around it'), nl,
   write('======================='), nl, nl,
   write('< / > / ^ / v : A horizontal or vertical edge of a ship.'), nl,
   write('- / |: The middle of a vertical or horizontal ship.'), nl,
   write('o: A one-piece ship.'), nl,
   write('~: Water.'), nl,
   write('?: Unknown.'), nl, nl,
   write('======================='), nl, nl.


solved_board :-
    ships_remaining(0),
    \+ (cell(_, _, unknown)),
    all_row_hints_satisfied,
    all_col_hints_satisfied,
    valid_no_touching.

all_row_hints_satisfied :-
    \+ (row_hint(R, Hint), findall(C, cell(R, C, ship(_,_,_)), L), length(L, N), N \= Hint).

all_col_hints_satisfied :-
    \+ (col_hint(C, Hint), findall(R, cell(R, C, ship(_,_,_)), L), length(L, N), N \= Hint).

choose_unknown_cell(R, C) :-
    cell(R, C, unknown), !.
    

% validation: no touching ships even diagonally

valid_no_touching :-
    \+ (
        cell(R, C, ship(_,_,_)),
        adjacent_neighbor(R, C, RN, CN),
        cell(RN, CN, ship(_,_,_)),
        ship_id(R, C, ID1),
        ship_id(RN, CN, ID2),
        ID1 \= ID2
    ).
    
check_user_solution :-
    (valid_no_touching ->
        write('No ships are touching. Board is valid by no-touching rule.'), nl
    ;
        write('Invalid: Some ships are touching!'), nl, fail
    ).

insert_ship_cell(R, C, '<') :- assert(cell(R, C, ship(horizontal,1,0))).
insert_ship_cell(R, C, '>') :- assert(cell(R, C, ship(horizontal,2,0))).
insert_ship_cell(R, C, '-') :- assert(cell(R, C, ship(horizontal,0,0))).
insert_ship_cell(R, C, '^') :- assert(cell(R, C, ship(vertical,1,0))).
insert_ship_cell(R, C, 'v') :- assert(cell(R, C, ship(vertical,2,0))).
insert_ship_cell(R, C, '|') :- assert(cell(R, C, ship(vertical,0,0))).
insert_ship_cell(R, C, 'o') :- assert(cell(R, C, ship(single, 1,0))).

validate_grid :-
    validate_row_hints,
    validate_column_hints,
    validate_no_touching_ships.

validate_row_hints :-
    \+ (row_hint(Row, Hint),
        findall(Col, (cell(Row, Col, ship(_,_,_)), is_ship_cell(Row, Col)), ShipCells),
        length(ShipCells, Count),
        Count \= Hint
    ).

validate_column_hints :-
    \+ (col_hint(Col, Hint),
        findall(Row, (cell(Row, Col, ship(_,_,_)), is_ship_cell(Row, Col)), ShipCells),
        length(ShipCells, Count),
        Count \= Hint
    ).

validate_no_touching_ships :-
    valid_no_touching.

is_ship_cell(Row, Col) :-
    cell(Row, Col, ship(_,_,_)).

set_water(Row, Col) :-
    retractall(cell(Row, Col, _)),
    assert(cell(Row, Col, water)).

user_place_ship(Row, Col, horizontal, Size) :-
    Size > 1,
    size(_, Cols),
    EndCol is Col + Size - 1,
    EndCol =< Cols,
    next_ship_id(ShipID),
    End is Size - 1,
    forall(between(0, End, I), (
        C is Col + I,
        % Remove any existing cell or ship state at this position
        retractall(cell(Row, C, _)),
        retractall(hidden_ship(Row, C)),
        retractall(ship_orientation(Row, C, _, _)),
        retractall(ship_id(Row, C, _)),
        Position is I + 1,
        assert(cell(Row, C, ship(horizontal, Position, ShipID))),
        assert(hidden_ship(Row, C)),
        assert(ship_orientation(Row, C, horizontal, Position)),
        assert(ship_id(Row, C, ShipID))
    )),
    NextID is ShipID + 1,
    retractall(next_ship_id(_)),
    assert(next_ship_id(NextID)),
    retractall(row_hint(_,_)),
    retractall(col_hint(_,_)),
    calculate_hints_from_board,
    count_ships_remaining,
    !.

user_place_ship(Row, Col, vertical, Size) :-
    Size > 1,
    size(Rows, _),
    EndRow is Row + Size - 1,
    EndRow =< Rows,
    next_ship_id(ShipID),
    End is Size - 1,
    forall(between(0, End, I), (
        R is Row + I,
        retractall(cell(R, Col, _)),
        retractall(hidden_ship(R, Col)),
        retractall(ship_orientation(R, Col, _, _)),
        retractall(ship_id(R, Col, _)),
        Position is I + 1,
        assert(cell(R, Col, ship(vertical, Position, ShipID))),
        assert(hidden_ship(R, Col)),
        assert(ship_orientation(R, Col, vertical, Position)),
        assert(ship_id(R, Col, ShipID))
    )),
    NextID is ShipID + 1,
    retractall(next_ship_id(_)),
    assert(next_ship_id(NextID)),
    retractall(row_hint(_,_)),
    retractall(col_hint(_,_)),
    calculate_hints_from_board,
    count_ships_remaining,
    !.

user_place_ship(Row, Col, single, 1) :-
    next_ship_id(ShipID),
    retractall(cell(Row, Col, _)),
    retractall(hidden_ship(Row, Col)),
    retractall(ship_orientation(Row, Col, _, _)),
    retractall(ship_id(Row, Col, _)),
    assert(cell(Row, Col, ship(single, 1, ShipID))),
    assert(hidden_ship(Row, Col)),
    assert(ship_orientation(Row, Col, single, 1)),
    assert(ship_id(Row, Col, ShipID)),
    NextID is ShipID + 1,
    retractall(next_ship_id(_)),
    assert(next_ship_id(NextID)),
    retractall(row_hint(_,_)),
    retractall(col_hint(_,_)),
    count_ships_remaining,
    !.
    
finalize_user_board :-
    retractall(row_hint(_,_)),
    retractall(col_hint(_,_)),
    calculate_hints_from_board,
    count_ships_remaining,
    !.

% this function fills the entire board with water and recalculate hints
fill_all_water :-
    size(Rows, Cols),
    forall(between(1, Rows, R),
        forall(between(1, Cols, C), (
            retractall(cell(R, C, _)),
            assert(cell(R, C, water))
        ))
    ),
    retractall(row_hint(_,_)),
    retractall(col_hint(_,_)),
    calculate_hints_from_board,
    count_ships_remaining.

% this function fills the entire board with ? unless it''s a ship
fill_all_unknown :-
    size(Rows, Cols),
    forall(between(1, Rows, R),
        forall(between(1, Cols, C), (
            retractall(cell(R, C, _)),
            assert(cell(R, C, unknown))
        ))
    ),
    default_board_size(Rows, Cols),
    assert(size(Rows, Cols)),
    calculate_hints_from_board,
    initialize_board,
    count_ships_remaining.

fill_unknowns_with_water :-
forall(cell(R, C, unknown), (
    retract(cell(R, C, unknown)),
    assert(cell(R, C, water))
)).