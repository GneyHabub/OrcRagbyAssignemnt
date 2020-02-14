%Check if item is on the list or not
on_list(Item,[Item|Rest]):-
    write(Item). 
on_list(Item,[DisregardHead|Tail]):-
    \+ Tail = [],
    on_list(Item,Tail).

%Iteration
iterate([]).
iterate([H|T]):- 
    write_to_file('output.txt', H), 
    write_to_file('output.txt', '\n'),
    iterate(T).

%Reading from file
read_file(Stream,[]) :-
    at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

%Writing to file1, X
write_to_file(File, Text):-
    open(File, append, Stream),
    write(Stream, Text),
    close(Stream).

whichLineIsOrc :-
        o(X, _),
        (
            X = 0 -> (write('Orc is on 0th line'));
            X = 1 -> (write('Orc is on 1st line'));
            X = 2 -> (write('Orc is on 2nd line'));
            X = 3 -> (write('Orc is on 3rd line'))
        ).

main :-
    randomSearch.

randomSearch :-
    consult('input.pl').
    random_between( 0, 3, X).
    format('~d', X).
    % o(X, _).