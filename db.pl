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

main :-
    open('input.txt', read, Str),
    read_file(Str,Lines),
    close(Str),
    iterate(Lines).