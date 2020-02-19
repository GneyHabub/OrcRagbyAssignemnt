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
    (X < 20), (X >= 0),
    (Y < 20), (Y >= 0).

notNearWall(c(X, Y)) :- 
    (X < 19), (X > 0),
    (Y < 19), (Y > 0).

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

visited(c(0, 0), s0).
visited(C, do(A, S)) :- 
    ball(C, do(A, S)); 
    visited(C, S).

isAdjacent(C, CN) :-
    cellRight(C, CN);
    cellBelow(C, CN);
    cellAbove(C, CN);
    cellLeft(C, CN).

randomDirection(C, AN) :-
    (
        (
            notNearWall(C),
            random(0, 4, Dir),
            (
                Dir = 0 -> AN = up;
                Dir = 1 -> AN = down;
                Dir = 2 -> AN = left;
                Dir = 3 -> AN = right
            ), !
        );
        (
            \+notNearWall(C),
            C = c(X, Y),
            (
                X = 0, Y = 0 -> (
                    random(0, 2, Dir),
                    (
                        Dir = 0 -> AN = up;
                        Dir = 1 -> AN = right
                    )
                );
                X = 0, Y = 19 -> (
                    random(0, 2, Dir),
                    (
                        Dir = 0 -> AN = down;
                        Dir = 1 -> AN = right
                    )
                );
                X = 19, Y = 19 -> (
                    random(0, 2, Dir),
                    (
                        Dir = 0 -> AN = left;
                        Dir = 1 -> AN = down
                    )
                );
                X = 19, Y = 0 -> (
                    random(0, 2, Dir),
                    (
                        Dir = 0 -> AN = up;
                        Dir = 1 -> AN = left
                    )
                );
                X = 0, Y \= 0, Y \= 19 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = up, !;
                        Dir = 1 -> AN = down, !;
                        (Dir = 2 -> AN = right, !)
                    )
                );
                X \= 0, X \= 19, Y = 0 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = up, !;
                        Dir = 1 -> AN = left, !;
                        (Dir = 2 -> AN = right, !)
                    )
                );
                X = 19, Y \= 19, Y \= 0 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = up, !;
                        Dir = 1 -> AN = left, !;
                        (Dir = 2 -> AN = down, !)
                    )
                );
                X \= 19, X \= 0, Y = 19 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = down, !;
                        Dir = 1 -> AN = left, !;
                        (Dir = 2 -> AN = right, !)
                    )
                )
            )
        )
    ). 
    
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
h(c(X, Y)) :-
    h(X, Y).

h(0, 3).
o(3, 2).
o(1, 2).
t(1, 3).

main :-
    consult('input.pl').
    randomSearch.

randomSearch :- 
    randomMove(ball(C, do(A, S0)), Count, Res),
    (
        Res = cont -> randomMove(ball())
    )


randomMove(ball(c(0, 0), s0), 0, cont).
randomMove(ball(C, do(A, S)), Count, Res) :-
    (
        Res = cont,
        randomMove(ball(C0, S0), Count0, cont),
        randomDirection(C0, A),
        ball(C, do(A, S0)),
        Count is Count0 + 1,
        (A = up; A = down; A = left; A = right),
        \+o(C),
        \+t(C),
        \+h(C)
    );
    (
        Res = cont,
        randomDirection(C0, A),
        ball(C, do(A, S0)),
        randomMove(ball(C0, S0), Count0, cont),
        Count is Count0 + 1,
        (A = up; A = down; A = left; A = right),
        o(C)
    );
    (
        Res = cont,
        randomDirection(C0, A),
        ball(C, do(A, S0)),
        randomMove(ball(C0, S0), Count0, cont),
        Count is Count0,
        (A = up; A = down; A = left; A = right),
        h(C)
    );
    (
        Res = cont,
        randomDirection(C0, A),
        ball(C, do(A, S0)),
        randomMove(ball(C0, S0), Count0, cont),
        Count is Count0 + 1,
        (A = up; A = down; A = left; A = right),
        t(C)
    ).
