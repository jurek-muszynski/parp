:- dynamic i_am_at/1, holding/1, at/2, in_dialog/1, die/0.
:- multifile initialize/1, options/1, slow_print/1.

import_source :-
    [source/locations],
    [source/items],
    [source/actions],
    [source/gameplay],
    [source/consts].

reset :-
    retractall(at(_, _)),
    retractall(i_am_at(_)),
    retractall(alive(_)),
    retractall(holding(_)),
    retractall(in_dialog(_)),
    assert(i_am_at(padded_cell)).

n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).

/* Ommit 'e' as it stands for 'east' */
a :- interact(0).
b :- interact(1).
c :- interact(2).
d :- interact(3).
f :- interact(4).
g :- interact(5).
h :- interact(6).
i :- interact(7).
j :- interact(8).
k :- interact(9).
l :- interact(10).

die :-
    reset,
    finish.

finish :-
    nl,
    write('The game is over.'),
    halt.

instructions :-
    nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('start.                  -- to start or reset the game.'), nl,
    write('n.  s.  e.  w.          -- to go in that direction.'), nl,
    write('a.  b.  c.  d.  ...     -- to select an interaction'), nl,
    write('drop(Object).           -- to put down an object.'), nl,
    write('inspect(Object).        -- to inspect an object.'), nl,
    write('look.                   -- to look around you again.'), nl,
    write('inventory.              -- to see what you are holding.'), nl,
    write('instructions.           -- to see this message again.'), nl,
    write('halt.                   -- to end the game and quit.'), nl,
    write('enter_code(Direction).  -- to enter the code for a door (surround it with parenthesis).'), nl,
    nl.

start :-
    import_source,
    reset,
    initialize(items),
    initialize(people),
    slow_print("You wake up in a padded cell. You don't remember how you got here, but you know you need to escape."), nl.
