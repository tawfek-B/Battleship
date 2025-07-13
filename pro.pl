:- dynamic cell/3.
:- dynamic row_hint/2.
:- dynamic col_hint/2.
:- dynamic size/2.
:- dynamic hidden_ship/2.
:- dynamic ships_remaining/1.
:- dynamic ship_orientation/4.
:- dynamic ship_id/3.
:- dynamic next_ship_id/1.
:- discontiguous place_random_ships/0.



default_board_size(8,8).


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


% This is an editable standard battleship fleet for 6x6 board
% We chose 1 ship of size 3, 2 ships of size 2, 3 ships of size 1.
standard_fleet([4, 3, 3, 3, 2, 2, 1, 1, 1, 1]).


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
    % Record orientation: horizontal ship, position in ship (1-based), total size
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
    % Record orientation: vertical ship, position in ship (1-based), total size
    Position is TotalSize - Size + 1,
    assert(ship_orientation(R, C, vertical, Position)),
    % Assign ship ID to track ship boundaries
    next_ship_id(ShipID),
    assert(ship_id(R, C, ShipID)),
    Size1 is Size - 1,
    R1 is R + 1,
    place_vertical_ship_cells(Size1, R1, C, TotalSize).



% now we have to check if we can place a ship at given position.
can_place_ship(R, C) :-
    \+ hidden_ship(R, C),
    % case 1: Position not already occupied
    \+ has_adjacent_ship(R, C),
    % case 2: No adjacent ships
    within_bounds(R, C).

% Checker to make sure if there are any orthogonal ships adjacent to the
% position ( diagonal ships are not accepted/allowed)
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
    print_board,
    solve.
% this function is just like the one before, but it varies in the
% solving proccess making it find the solution while printing the board
% after each step instead of prinitng the boead just once when the
% soltution is found.
run_steps(N) :-
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
% at first we check the remaining ships to know how many of them need to
% be found, if there is/are ship/s to be found then we continue the
% solving proccess, if ther is not then just end it.
solve_loop :-
    ships_remaining(Remaining),
    Remaining > 0,
    apply_rules_once,
    print_board,
    solve_loop.
solve_loop :-
    ships_remaining(0),
    write('Solved! All ships found.'), nl.


% just the same as the ones before but made to be called after each step
% made diuring the proccess.
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
    (Remaining =:= 0 ->
        write('All ships found!'), nl
    ;   apply_rules_once,
        print_board,
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


% this is the method that prints the board on console.
% by printing the board as it is like empty (all sea at the beginning)
% with hint numbers for each row and column printed beside it.
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


% just the things that will be printed for each case, if it is a sea
% then print "~", if ship print directional arrow and if it is not known for now then
% it will be printed as "?".
print_cell(ship(Orientation, Position, ShipID)) :-
    print_ship_arrow(Orientation, Position, ShipID).
print_cell(water) :- write('   ~').
print_cell(unknown) :- write('   ?').

% Print arrows for ships based on orientation, position, and ship ID
print_ship_arrow(horizontal, 1, ShipID) :-
    % Check if this is a single-part ship by counting parts of this specific ship
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    (ShipSize = 1 ->
        write('   ●')  % Circle for single-part ships
    ;   write('   <')  % Left arrow for first part of multi-part horizontal ship
    ).
print_ship_arrow(horizontal, Position, ShipID) :-
    % Check if this is the last part of this specific ship
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    Position = ShipSize,
    write('   >').  % Right arrow for last part of horizontal ship
print_ship_arrow(horizontal, _, _) :- write('   -').  % Dash for middle parts of horizontal ship
print_ship_arrow(vertical, 1, ShipID) :-
    % Check if this is a single-part ship by counting parts of this specific ship
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    (ShipSize = 1 ->
        write('   ●')  % Circle for single-part ships
    ;   write('   ^')  % Up arrow for first part of multi-part vertical ship
    ).
print_ship_arrow(vertical, Position, ShipID) :-
    % Check if this is the last part of this specific ship
    findall(1, ship_id(_, _, ShipID), ShipParts),
    length(ShipParts, ShipSize),
    Position = ShipSize,
    write('   v').  % Down arrow for last part of vertical ship
print_ship_arrow(vertical, _, _) :- write('   |').   % Pipe for middle parts of vertical ship


% --------------------

within_bounds(R,C) :-
    size(Rows, Cols),
    R >= 1, R =< Rows,
    C >= 1, C =< Cols.

random_member(X, List) :-
    length(List, Len),
    Len > 0,
    random(0, Len, Index),
    nth0(Index, List, X).

% Print instructions when file is consulted
:- write('=== Battleship Solver ==='), nl,
   write('Available commands:'), nl,
   write('  run.           - Solve after initializing board'), nl,
   write('  run_steps(N).  - Solve with N steps, without initalizing'), nl,
   write('  init.          - Initialize a new boad'), nl,
   write('  print_board.   - Print the current board'), nl,
   write('======================='), nl, nl,
   write('< / > / ^ / v : A horizontal or vertical edge of a ship.'), nl,
   write('- / |: The middle of a vertical or horizontal ship.'), nl,
   write('●: A one-piece ship.'), nl,
   write('~: Water.'), nl,
   write('?: Unknown.'), nl, nl,
   write('======================='), nl, nl.
