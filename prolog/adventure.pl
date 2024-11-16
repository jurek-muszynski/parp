:- dynamic i_am_at/1, holding/1, at/2, in_dialog/1.
:- multifile initialize/1, options/1.

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
    finish.

finish :-
    nl,
    write('The game is over. Please enter the "halt." command.'),
    nl.

check_code(X) :-
    i_am_at(X),
    encoded_door(X, _, _),
    write('The door is locked. Enter the code: '),
    read(InputCode),
    code(RequiredCode),
    (   InputCode = RequiredCode
    ->  write('The code is correct. The door unlocks!'), nl,
        assert(entered_code(X))
    ;   write('Incorrect code. The door remains locked.'), nl
    ).

check_code(_) :-
    write('There is nowhere you could enter the code here.'), nl.

instructions :-
    nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('start.                   -- to start or reset the game.'), nl,
    write('n.  s.  e.  w.           -- to go in that direction.'), nl,
    write('a.  b.  c.  d.  ...      -- to select an interaction'), nl,
    % write('take(Object).            -- to pick up an object.'), nl,
    write('drop(Object).            -- to put down an object.'), nl,
    write('inspect(Object).         -- to inspect an object.'), nl,
    write('look.                    -- to look around you again.'), nl,
    % write('talk_to(Person).         -- to talk to a person.'), nl,
    % write('ask(Person, Question).   -- to ask a person a question.'), nl,
    write('inventory.               -- to see what you are holding.'), nl,
    write('instructions.            -- to see this message again.'), nl,
    write('halt.                    -- to end the game and quit.'), nl,
    write('check_code(Location).    -- to check the code for a door.'), nl,
    nl.

start :-
    import_source,
    reset,
    initialize(items),
    initialize(people),
    write("You wake up in a padded cell. You don't remember how you got here, but you know you need to escape."), nl.
