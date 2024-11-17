:- multifile write_dialog_options/1.

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

/* This rule tells how to interact with people and items */

interact(Option) :-
    \+ in_dialog(_),
    i_am_at(Place),
    findall(X, at(X, Place), Entities),
    (nth0(Option, Entities, Entity) ->
        (is_person(Entity) -> talk_to(Entity) ; take(Entity))
        ; (write('No such interaction.'), nl)
    ), !.

interact(Option) :-
    in_dialog(Person),
    findall(Line, dialog_line(Person, Line, _), Lines),
    (nth0(Option, Lines, DialogLine) ->
        (dialog_line(Person, DialogLine, Response), Response)
        ; (write('No such dialog line.'), nl)
    ), !.

/* This rule tells how to look around you. */

look :-
    i_am_at(Place),
    describe(Place),
    nl,
    notice_at(Place).

notice_at(Place) :-
    findall(X, at(X, Place), Entities),
    write_options(Entities),
    fail.

notice_at(_).

write_options([]).
write_options(Xs) :-
    Xs \= [],
    write('Interactions: '), nl,
    options(Options),
    write_options(Xs, Options), nl.

write_options([], _).
write_options(_, []).
write_options([X|Xs], [Letter|Letters]) :-
    write('  ('), write(Letter), write(') '),
    (is_person(X) -> write('Talk to the ') ; write('Pick up the ')),
    write(X), nl,
    write_options(Xs, Letters).

write_dialog_options(Xs) :-
    write('Dialog options: '), nl,
    options(Options),
    write_dialog_options(Xs, Options).

write_dialog_options([], _).
write_dialog_options([X|Xs], [Letter|Letters]) :-
    write(' ('), write(Letter), write(') '), write(X), nl,
    write_dialog_options(Xs, Letters).

/* This rule tells you to look at your inventory. */

write_items([]).
write_items([X|Xs]) :-
    write(' --> '), write(X), nl,
    write_items(Xs).

inventory :- 
    findall(Item, holding(Item), Items),
    write_items(Items).
