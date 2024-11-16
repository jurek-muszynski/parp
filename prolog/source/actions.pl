:- dynamic at/2.
:- multifile initialize/1.

initialize(people) :-
    assert(at(receptionist, reception)),
    assert(at(doctor, exam_room1)).

talk_to(doctor) :-
    i_am_at(exam_room1),
    write('The doctor looks up from his desk and says, "Hi I how can I help you"'), nl,
    write('You can ask him for "discharge"'), nl,
    !, nl.

talk_to(receptionist) :-
    i_am_at(reception),
    write("The receptionist looks up from her computer and says, 'Hi, how can I help you?'"), nl,
    write('You can ask her about the "bus departure"'), nl,
    !, nl.

ask(doctor, "discharge") :-
    i_am_at(exam_room1),
    holding(discharge_form),
    write('The doctor looks at you and says, "Yes, you are ready for discharge. I will have the receptionist prepare the paperwork."'), nl,
    assert(holding(discharge_approval)),
    !, nl.

ask(doctor, "discharge") :-
    i_am_at(exam_room1),
    \+     holding(discharge_form),
    write('The doctor looks at you and says, "I cannot discharge you until you have the proper paperwork. Please find your discharge form."'), nl,
    write('The doctor has just sent you back to your padded cell and collected all of your belongings.'), nl,
    retractall(holding(_)),
    retract(i_am_at(exam_room1)),
    assert(i_am_at(padded_cell)),
    !, nl.

ask(receptionist, "bus departure") :-
    i_am_at(reception),
    holding(discharge_approval),
    write("The receptionist looks at you and says, 'The bus departs at 5:00 PM'"), nl,
    !, nl.

ask(receptionist, "bus departure") :-
    i_am_at(reception),
    \+     holding(discharge_approval),
    write("The receptionist looks at you apologetically and says, 'I'm sorry, but I can't give you that information until the doctor has discharged you from the hospital.'"), nl,
    !, nl.
