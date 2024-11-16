:- dynamic at/2.
:- multifile initialize/1.

/* Items at locations */
initialize(items) :-
    assert(at(padded_cell_key, padded_cell)),
    assert(at(exit_key, basement)),
    assert(at(patient_file, exam_room1)),
    assert(at(discharge_form, restroom)),
    assert(at(set_of_notes, exam_room2)).

/* Items capabilities */
can_unlock(door(padded_cell, s, hallway_one)) :- holding(padded_cell_key).
can_unlock(door(reception, e, exit)) :- holding(exit_key).
can_unlock(encoded_door(hallway_two, w, basement)) :- entered_code(hallway_two).

/* Items descriptions */
describe(padded_cell_key) :-
    write('A small, slightly rusted key that opens the door to the padded cell. It feels cold to the touch and has a faint engraving on it.'), nl.

describe(exit_key) :-
    write('A sturdy key that opens the exit door. It has a unique shape and seems to be well-used, indicating frequent use.'), nl.

describe(patient_file) :-
    write('A file containing detailed information about a patient. The pages are worn and some notes are scribbled in the margins, hinting at a troubled history.'), nl,
    write('Patient File - Patient 12:'), nl,
    write('Name: Unknown'), nl,
    write('Date of Birth: 01.01.1990'), nl.
    
describe(set_of_notes) :-
    write('A set of handwritten notes, filled with observations and theories.'), nl,
    write('The handwriting is hurried and some parts are difficult to read, but they seem to contain important information.'), nl,
    write('Report - 1/2/2020: Patient 12 is showing signs of improvement. The new treatment seems to be working.'), nl,
    write('Report - 1/3/2020: Patient 12 is showing signs of distress. The new treatment is causing unexpected side effects.'), nl,
    write('Report - 1/4/2020: Patient 12 is unresponsive. The new treatment has failed. The patient is to be moved to the basement for further observation'), nl,
    write('Report - 1/5/2020: Patient 12 has been moved to the basement. The code is his date of birth, The patient is to be monitored closely.'), nl.

describe(discharge_form) :-
    write('A form that seems to be a discharge form. It is filled out with a patient\'s information and seems to be ready for processing.'), nl,
    write('Discharge Form - Patient 12:'), nl,
    write('Name: Unknown'), nl,
    write('Date of discharge: 31.02.2020'), nl.
