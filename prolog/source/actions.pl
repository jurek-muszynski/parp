:- dynamic at/2.
:- multifile initialize/1.


initialize(people) :-
    assert(at(receptionist, reception)),
    assert(at(doctor, exam_room1)).

is_person(doctor).
is_person(receptionist).

talk_to(Person) :-
    person_descr(Person),
    findall(Line, dialog_line(Person, Line, _), DialogLines),
    write_dialog_options(DialogLines),
    assert(in_dialog(Person)),
    !.

person_descr(doctor) :-
    slow_print('The doctor looks up from his desk and says, "Hi how can I help you"'), nl.
person_descr(receptionist) :-
    slow_print("The receptionist looks up from her computer and says, 'Hi, how can I help you?'"), nl.

dialog_line(doctor, 'Ask for discharge.', ask(doctor, discharge)).
dialog_line(receptionist, 'Ask about the bus', ask(receptionist, bus_departure)).

ask(doctor, discharge) :-
    holding(discharge_form),
    slow_print('The doctor looks at you and says, "Yes, you are ready for discharge. I will have the receptionist prepare the paperwork."'), nl,
    assert(holding(discharge_approval)),
    retract(in_dialog(doctor)),
    !, nl.

ask(doctor, discharge) :-
    \+ holding(discharge_form),
    slow_print('The doctor looks at you and says, "I cannot discharge you until you have the proper paperwork. Please find your discharge form."'), nl,
    slow_print('The doctor has just sent you back to your padded cell and collected all of your belongings.'), nl,
    start,
    !, nl.

ask(receptionist, bus_departure) :-
    holding(discharge_approval),
    slow_print("The receptionist looks at you and says, 'The bus departs at 5:00 PM'"), nl,
    retract(in_dialog(receptionist)),
    !, nl.

ask(receptionist, bus_departure) :-
    \+ holding(discharge_approval),
    slow_print("The receptionist looks at you apologetically and says, 'I'm sorry, but I can't give you that information until the doctor has discharged you from the hospital.'"), nl,
    retract(in_dialog(receptionist)),
    !, nl.

slow_print(Text) :-
    string_chars(Text, Chars),
    maplist(print_with_delay, Chars).

print_with_delay(Char) :-
    write(Char),
    flush_output,
    sleep(0.05).
