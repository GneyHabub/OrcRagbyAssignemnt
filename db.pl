ball(c(0, 0), s0).
ball(C, do(A, S)) :- 
    ball(C0, S),
    (
        (A = up, cellAbove(C0, C), noWall(C));
        (A = down, cellBelow(C0, C), noWall(C));
        (A = left, cellLeft(C0, C), noWall(C));
        (A = right, cellRight(C0, C), noWall(C))
    ).

cellAbove(c(X0, Y0), c(XN, YN)) :-
    XN is X0, YN is Y0+1.
cellBelow(c(X0, Y0), c(XN, YN)) :-
    XN is X0, YN is Y0-1.
cellLeft(c(X0, Y0), c(XN, YN)) :-
    XN is X0-1, YN is Y0.
cellRight(c(X0, Y0), c(XN, YN)) :-
    XN is X0+1, YN is Y0.

noWall(c(X, Y)) :-
    (X < 4), (X >= 0),
    (Y < 5), (Y >= 0).

ballNotLost(s0).
ballNotLost(do(A, S)) :-
    ballNotLost(S),
    ball(C, do(A, S)),
    \+o(C).

canPass(s0).
canPass(do(A, S)) :-
    canPass(S);
    A = pass -> false.

touchDown(s0) :- false.
touchDown(do(A, S)) :- 
    (ball(C, do(A, S)),
    t(C)).

orcNearBy(S) :-
    ball(C, S),
    o(CN),
    isAdjacent(C, CN).

isAdjacent(C, CN) :-
    cellRight(C, CN);
    cellBelow(C, CN);
    cellAbove(C, CN);
    cellLeft(C, CN).














main :-
    randomSearch.

randomSearch :-
    consult('input.pl').
    
%HELPERS
%Adds element to list
add(E,L,[E|L]).

%Counts number of elements in list
countList([],0). 
countList([_|Tail], N) :- countList(Tail, N1), N is N1 + 1.

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

%Conver to the used format
o(c(X, Y)) :-
    o(X, Y).
t(c(X, Y)) :-
    t(X, Y).