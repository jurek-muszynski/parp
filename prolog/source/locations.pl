:- multifile path/3, describe/1, door/3, encoded_door/3, slow_print/1.
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

path(exit, e, reception).

path(exam_room1, w, hallway_two).

path(exam_room2, n, hallway_two).

path(basement, e, hallway_two).

/* Doors between locations */

door(padded_cell, s, hallway_one).
door(reception, w, exit).

encoded_door(hallway_two, w, basement).

bus(exit, s, bus_stop).

/* These are locations descriptions */

describe(reception) :-
    write('You are at the reception.'), nl,
    write('It is mostly empty, apart from a few flower pots set against the walls and a desk in the middle.'), nl,
    write('Behind it sits a middle-aged woman, with her face illuminated from a computer screen just before her eyes.'), nl,
    write('If she looked over the screen, she would set her eyes on a door with a phosphorescent sign that reads "EXIT".'), nl,
    write('On the wall behind her closer to the corner of the room there is another door with sign,'), nl,
    write('This time depicting two silhouettes - male and female.'), nl,
    write('n - hallway'), nl,
    write('s - hallway'), nl,
    write('e - restroom'), nl,
    write('w - door to the exit'), nl.

describe(padded_cell) :- 
    write('You are in a padded cell.'), nl,
    write('The walls are covered in a soft, padded material.'), nl,
    write('There are no windows, the only connection with the outside world is a heavy door'), nl,
    (once(at(padded_cell_key, padded_cell)) -> write('You notice a small key in the corner of the room.'), nl; true),
    write('s - door to the hallway'), nl.

describe(hallway_one) :-
    write('You are in the first part of the hallway.'), nl,
    write('The walls here are lined with faded paintings of serene landscapes and old photographs of what appears to be hospital staff.'), nl,
    write('A faint smell of antiseptic lingers in the air, blending with a subtle metallic undertone.'), nl,
    write('The floor is covered with white linoleum, polished to a dull shine.'), nl,
    write('Every so often, faint whispers and footsteps echo from unseen corners, sending a chill down your spine.'), nl,
    write('n - padded cell'), nl,
    write('s - reception'), nl.

describe(hallway_two) :-
    write('You are in the second part of the hallway.'), nl,
    write('This hallway is dimly lit, with a few fluorescent lights flickering overhead, casting eerie shadows on the walls.'), nl,
    write('The walls are painted a dull, institutional green, with patches of peeling paint revealing bare concrete underneath.'), nl,
    write('A door marked "Exam Room 2" looms ominously to the south, and to the east, the slightly ajar door to Exam Room 1 reveals a glimpse of what lies beyond.'), nl,
    write('To the west, a heavy steel door with a rusted sign reading "Basement" emanates a low hum of machinery.'), nl,
    write('n - reception'), nl,
    write('s - exam room 2'), nl,
    write('e - exam room 1'), nl,
    write('w - basement door'), nl.

describe(exam_room1) :-
    write('You are in Exam Room 1.'), nl,
    write('This room is stark and clinical, with a single examination table at its center covered in worn white sheets.'), nl,
    write('A cabinet filled with dusty medical equipment and supplies stands in the corner, its glass doors slightly cracked.'), nl,
    write('The air feels heavy, and there\'s a faint smell of alcohol and old paper lingering here.'), nl,
    (once(at(patient_file, exam_room1)) -> write('You notice a desk with a pile of documents and a medical file on top.'), nl; true),
    write('w - hallway'), nl.

describe(exam_room2) :-
    write('You are in Exam Room 2.'), nl,
    write('This room is eerily silent. The examination table is overturned, and scattered papers litter the floor.'), nl,
    write('Broken glass glistens faintly in the dim light, and a single chair sits in the corner, its back turned toward you.'), nl,
    write('There\'s a faint coppery smell in the air that makes you uneasy.'), nl,
    (once(at(set_of_notes, exam_room2)) -> write('You notice a set of notes on the floor, partially hidden under the table.'), nl; true),
    write('n - hallway'), nl.

describe(basement) :-
    write('You are in the basement.'), nl,
    write('The air here is cold and damp, with the faint hum of machinery echoing throughout the space.'), nl,
    write('The walls are lined with exposed pipes, some of them dripping water onto the concrete floor.'), nl,
    write('In the faint light, you can see scattered tools and boxes, but the place feels foreboding, as if it holds secrets best left undiscovered.'), nl,
    (once(at(exit_key, basement)) -> write('You notice a key on the floor, glinting in the dim light.'), nl; true),
    write('e - hallway'), nl.

describe(restroom) :-
    write('You are in the restroom.'), nl,
    write('The restroom is small and sterile, with cracked mirrors above the sinks and water dripping slowly from a leaky faucet.'), nl,
    write('The faint smell of cleaning chemicals lingers in the air, mixed with something more foul from an unseen corner.'), nl,
    write('The tiled floor is slick, reflecting the dim light from a single bulb overhead.'), nl,
    (once(at(discharge_form, restroom)) -> write('You notice a form on the sink, partially filled out.'), nl; true),
    write('w - reception'), nl.

describe(exit) :-
    write('You are at the exit.'), nl,
    write('A small panel beside the door looks like it might require a key or code to unlock.'), nl,
    write('To the east, the faint glow from the reception area is visible.'), nl,
    write('To the south, a narrow path leads to a bus stop outside.'), nl,
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
