:- multifile describe/1, path/3, notice_objects_at/1.

describe(reception) :-
    write('You walk into the reception room.'), nl,
    write('It is a medium-sized room in shape of a semicircle with an empty desk'), nl,
    write('positioned centrally against the straight wall. A few scattered documents'), nl,
    write('with coffee stains, a pen and a coffee cup, guilty of taking away the pure white'), nl,
    write('of freshly printed paper - beyond that the desk is glaringly clean.'), nl,
    write('The desk, facing towards a gap in the middle of the curved wall where a long hallway'), nl,
    write('stretches into the distance, resembles an outpost guarding'), nl,
    write('what is behind from what is upfront.'), nl,
    write('In the dark corner of the room, initially shielded by the dim light, a door appears.'), nl,
    write('On it, barely visible, hangs a sign depicting two people of opposing genders'), nl,
    write('- a restroom. The ajared door seems inexplicably inviting, as if the alter ego wished'), nl,
    write('to peek inside.'), nl.

path(reception, s, hallway).
path(reception, w, restroom).

notice_objects_at(reception) :- todo. /* TODO */
