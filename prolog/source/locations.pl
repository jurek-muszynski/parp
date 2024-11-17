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
path(exit, s, bus_stop).

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
    write('It is mostly empty, apart from a few flower pots set against the walls and a desk in the middle.'), nl,
    write('Behind it sits a middle-aged woman, with her face illuminated from a computer screen just before her eyes.'), nl,
    write('If she looked over the screen, she would set her eyes on a door with a phosphorescent sign that reads "EXIT".'), nl,
    write('On the wall behind her closer to the corner of the room there is another door with sign,'), nl,
    write('this time depicting two silhouettes - male and female.'), nl,
    write('n - hallway'), nl,
    write('s - hallway'), nl,
    write('e - restroom'), nl,
    write('w - door to the exit'), nl.

describe(padded_cell) :- 
    write('You are in a padded cell.'), nl,
    write('The walls are covered in a soft, padded material.'), nl,
    write('There are no windows, the only connection with the outside world is a heavy door'), nl,
    (at(padded_cell_key, padded_cell) -> write('You notice a small key in the corner of the room.'), nl),
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
    write('e - reception'), nl,
    write('s - bus stop'), nl.

describe(bus_stop) :-
    nl,
    slow_print("You step out of the asylum into the crisp evening air."), nl,
    slow_print("The world feels strange, almost unfamiliar."), nl,
    slow_print("You walk to the nearest bus stop and sit down."), nl,
    nl,
    slow_print("A bus arrives. The sign reads: 'Bialystok'."), nl,
    slow_print("You board the bus, unsure of what lies ahead, but hopeful."), nl,
    nl,
    slow_print("The engine hums as the bus drives away, taking you toward a new beginning..."), nl,
    nl,
    slow_print("*** THE END ***"), nl.

slow_print(Text) :-
    string_chars(Text, Chars),
    maplist(print_with_delay, Chars).

print_with_delay(Char) :-
    write(Char),
    flush_output,
    sleep(0.05).
