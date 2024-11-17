:- multifile path/3, describe/1, door/3, encoded_door/3.

/* Paths between locations */

path(hallway_two, n, reception).
path(hallway_two, s, exam_room2).
path(hallway_two, e, exam_room1).

path(reception, n, hallway_one).
path(reception, s, hallway_two).
path(reception, e, restroom).

path(hallway_one, n, padded_cell).
path(hallway_one, s, reception).

path(restroom, w, reception).

path(exit, w, reception).

path(exam_room1, w, hallway_two).

path(exam_room2, n, hallway_two).

path(basement, e, hallway_two).

/* Doors between locations */

door(padded_cell, s, hallway_one).
door(reception, w, exit).

encoded_door(hallway_two, w, basement).

/* These are locations descriptions */

describe(reception) :-
    write('You are in the reception.'), nl,
    write('n - hallway'), nl,
    write('s - hallway'), nl,
    write('e - restroom'), nl,
    write('w - door to the exit'), nl.

describe(padded_cell) :- 
    write('You are in a padded cell.'), nl,
    write('The walls are covered in a soft, padded material.'), nl,
    write('s - door to the hallway'), nl.

describe(hallway_one) :-
    write('You are in a hallway (hallway_one).'), nl,
    write('n - padded cell'), nl,
    write('s - reception'), nl.

describe(hallway_two) :-
    write('You are in a hallway (hallway_two).'), nl,
    write('n - reception'), nl,
    write('s - exam room 2'), nl,
    write('e - exam room 1'), nl,
    write('w - door to the basement'), nl.

describe(exam_room1) :-
    write('You are in an exam room (exam_room1).'), nl,
    write('w - hallway'), nl.

describe(exam_room2) :-
    write('You are in an exam room (exam_room2).'), nl,
    write('n - hallway'), nl.

describe(basement) :-
    write('You are in the basement.'), nl,
    write('e - hallway'), nl.

describe(restroom) :-
    write('You are in the restroom.'), nl,
    write('w - reception'), nl.

describe(exit) :-
    write('You are at the exit.'), nl,
    write('e - reception'), nl.
