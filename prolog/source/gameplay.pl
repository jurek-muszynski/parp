/* These rules describe how to pick up an object. */

take(X) :-
    holding(X),
    write('You''re already holding it!'),
    !, nl.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    retract(at(X, Place)),
    assert(holding(X)),
    write('OK.'),
    !, nl.

take(_) :-
    write('I don''t see it here.'),
    nl.

/* These rules describe how to put down an object. */

drop(X) :-
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assert(at(X, Place)),
    write('OK.'),
    !, nl.

drop(_) :-
    write('You aren''t holding it!'),
    nl.

/* These rules describe how to inspect an object. */
inspect(X) :-
    i_am_at(Place),
    at(X, Place),
    describe(X),
    !, nl.

inspect(X) :-
    holding(X),
    describe(X),
    !, nl.

inspect(_) :-
    write('I don''t see it here.'),
    nl.

/* This rule tells how to move in a given direction. */

go(Direction) :-
    i_am_at(Here),
    path(Here, Direction, There),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    !, look.

go(Direction) :-
    i_am_at(Here),
    door(Here, Direction, There),
    can_unlock(door(Here, Direction, There)),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    !, look.

go(Direction) :-
    i_am_at(Here),
    door(Here, Direction, There),
    \+ can_unlock(door(Here, Direction, There)),
    write('The door is locked. You need a key to enter.'),
    !, nl.

go(Direction) :-
    i_am_at(Here),
    encoded_door(Here, Direction, There),
    can_unlock(encoded_door(Here, Direction, There)),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    !, look.

go(Direction) :-
    i_am_at(Here),
    encoded_door(Here, Direction, There),
    \+ can_unlock(encoded_door(Here, Direction, There)),
    write('The door is locked. You need a code to enter.'),
    !, nl.

go(_) :-
    write("You can't go that way.").

/* This rule tells how to look around you. */

look :-
    i_am_at(Place),
    describe(Place),
    nl,
    notice_objects_at(Place),
    nl.

/* These rules set up a loop to mention all the objects
in your vicinity. */

notice_objects_at(Place) :-
    at(X, Place),
    write('There is a '), write(X), write(' here.'), nl,
    fail.

notice_objects_at(_).

notice_people_at(Place) :-
    at(X, Place),
    write('There is a '), write(X), write(' here.'), nl,
    fail.

notice_people_at(_).

/* This rule tells you to look at your inventory. */

inventory :-
    \+ (holding(_)) ->
        write('You are empty handed.'), nl; 
        (write('You have: '), nl, (holding(X), write(' --> '), write(X), nl, fail ; true)).
