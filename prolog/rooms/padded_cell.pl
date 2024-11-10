:- multifile describe/1, door/3, notice_objects_at/1.
:- multifile at/2.

describe(padded_cell) :- write('Description: TODO'), nl.

door(padded_cell, w, reception).
can_unlock(door(padded_cell, w, reception)) :- holding("padded cell key").

at("padded cell key", padded_cell).

notice_objects_at(reception) :- write('Description: TODO'), nl.
