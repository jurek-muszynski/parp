:- dynamic i_am_at/1, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).
:- multifile at/2.

:- [source/locations].
:- [source/items].
:- [source/actions].
:- [source/gameplay].
:- [source/consts].

i_am_at(padded_cell).

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

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
    write('start.                   -- to start the game.'), nl,
    write('n.  s.  e.  w.           -- to go in that direction.'), nl,
    write('take(Object).            -- to pick up an object.'), nl,
    write('drop(Object).            -- to put down an object.'), nl,
    write('inspect(Object).         -- to inspect at an object.'), nl,
    write('look.                    -- to look around you again.'), nl,
    write('talk_to(Person).         -- to talk to a person.'), nl,
    write('ask(Person, Question).   -- to ask a person a question.'), nl,
    write('inventory.               -- to see what you are holding.'), nl,
    write('instructions.            -- to see this message again.'), nl,
    write('halt.                    -- to end the game and quit.'), nl,
    write('check_code(Location).    -- to check the code for a door.'), nl,
    nl.

start :-
    write("You wake up in a padded cell. You don't remember how you got here, but you know you need to escape."), nl.
