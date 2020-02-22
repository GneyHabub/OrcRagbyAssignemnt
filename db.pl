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
    (X < 7), (X >= 0),
    (Y < 7), (Y >= 0).

notNearWall(c(X, Y)) :- 
    (X < 6), (X > 0),
    (Y < 6), (Y > 0).

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
                X = 0, Y = 6 -> (
                    random(0, 2, Dir),
                    (
                        Dir = 0 -> AN = down;
                        Dir = 1 -> AN = right
                    )
                );
                X = 6, Y = 6 -> (
                    random(0, 2, Dir),
                    (
                        Dir = 0 -> AN = left;
                        Dir = 1 -> AN = down
                    )
                );
                X = 6, Y = 0 -> (
                    random(0, 2, Dir),
                    (
                        Dir = 0 -> AN = up;
                        Dir = 1 -> AN = left
                    )
                );
                X = 0, Y \= 0, Y \= 6 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = up, !;
                        Dir = 1 -> AN = down, !;
                        (Dir = 2 -> AN = right, !)
                    )
                );
                X \= 0, X \= 6, Y = 0 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = up, !;
                        Dir = 1 -> AN = left, !;
                        (Dir = 2 -> AN = right, !)
                    )
                );
                X = 6, Y \= 6, Y \= 0 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = up, !;
                        Dir = 1 -> AN = left, !;
                        (Dir = 2 -> AN = down, !)
                    )
                );
                X \= 6, X \= 0, Y = 6 -> (
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

randomPass(C, AN) :-
    (
        (
            notNearWall(C),
            random(0, 8, Dir),
            (
                Dir = 0 -> AN = up;
                Dir = 1 -> AN = down;
                Dir = 2 -> AN = left;
                Dir = 3 -> AN = right;
                Dir = 4 -> AN = up_right;
                Dir = 5 -> AN = down_right;
                Dir = 6 -> AN = down_left;
                Dir = 7 -> AN = up_left
            ), !
        );
        (
            \+notNearWall(C),
            C = c(X, Y),
            (
                X = 0, Y = 0 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = up;
                        Dir = 1 -> AN = right;
                        Dir = 2 -> AN = up_right
                    )
                );
                X = 0, Y = 6 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = down;
                        Dir = 1 -> AN = right;
                        Dir = 2 -> AN = down_right
                    )
                );
                X = 6, Y = 6 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = left;
                        Dir = 1 -> AN = down;
                        Dir = 2 -> AN = down_left
                    )
                );
                X = 6, Y = 0 -> (
                    random(0, 3, Dir),
                    (
                        Dir = 0 -> AN = up;
                        Dir = 1 -> AN = left;
                        Dir = 2 -> AN = up_left
                    )
                );
                X = 0, Y \= 0, Y \= 6 -> (
                    random(0, 5, Dir),
                    (
                        Dir = 0 -> AN = up;
                        Dir = 1 -> AN = down;
                        Dir = 2 -> AN = right;
                        Dir = 3 -> AN = up_right;
                        Dir = 4 -> AN = down_right
                    )
                );
                X \= 0, X \= 6, Y = 0 -> (
                    random(0, 5, Dir),
                    (
                        Dir = 0 -> AN = up;
                        Dir = 1 -> AN = left;
                        Dir = 2 -> AN = right;
                        Dir = 3 -> AN = up_left;
                        Dir = 4 -> AN = up_right
                    )
                );
                X = 6, Y \= 6, Y \= 0 -> (
                    random(0, 5, Dir),
                    (
                        Dir = 0 -> AN = up;
                        Dir = 1 -> AN = left;
                        Dir = 2 -> AN = down;
                        Dir = 3 -> AN = up_left;
                        Dir = 4 -> AN = down_left
                    )
                );
                X \= 6, X \= 0, Y = 6 -> (
                    random(0, 5, Dir),
                    (
                        Dir = 0 -> AN = down;
                        Dir = 1 -> AN = left;
                        Dir = 2 -> AN = right;
                        Dir = 3 -> AN = down_right;
                        Dir = 4 -> AN = down_left
                    )
                )
            )
        )
    ).

toss(c(X, Y), A, CN) :-
    A = up -> (
        noWall(c(X, Y + 1)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X, Y + 1), A, CN));
        noWall(c(X, Y + 1)), o(c(X, Y)) -> (false);
        noWall(c(X, Y + 1)), h(c(X, Y)) -> (CN = c(X, Y + 1));
        \+noWall(c(X, Y + 1)) -> (false)
    );
    A = down -> (
        noWall(c(X, Y - 1)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X, Y - 1), A, CN));
        noWall(c(X, Y - 1)), o(c(X, Y)) -> (false);
        noWall(c(X, Y - 1)), h(c(X, Y)) -> (CN = c(X, Y + 1));
        \+noWall(c(X, Y - 1)) -> (false)
    );
    A = left -> (
        noWall(c(X-1, Y)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X - 1, Y), A, CN));
        noWall(c(X-1, Y)), o(c(X, Y)) -> (false);
        noWall(c(X-1, Y)), h(c(X, Y)) -> (CN = c(X, Y + 1));
        \+noWall(c(X-1, Y)) -> (false)
    );
    A = right -> (
        noWall(c(X+1, Y)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X+1, Y), A, CN));
        noWall(c(X+1, Y)), o(c(X, Y)) -> (false);
        noWall(c(X+1, Y)), h(c(X, Y)) -> (CN = c(X, Y + 1));
        \+noWall(c(X+1, Y)) -> (false)
    );
    A = up_right -> (
        noWall(c(X+1, Y+1)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X, Y + 1), A, CN));
        noWall(c(X+1, Y+1)), o(c(X, Y)) -> (false);
        noWall(c(X+1, Y+1)), h(c(X, Y)) -> (CN = c(X, Y + 1));
        \+noWall(c(X+1, Y+1)) -> (false)
    );
    A = down_right -> (
        noWall(c(X+1, Y-1)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X+1, Y - 1), A, CN));
        noWall(c(X+1, Y-1)), o(c(X, Y)) -> (false);
        noWall(c(X+1, Y-1)), h(c(X, Y)) -> (CN = c(X, Y + 1));
        \+noWall(c(X+1, Y-1)) -> (false)
    );
    A = down_left -> (
        noWall(c(X-1, Y-1)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X-1, Y-1), A, CN));
        noWall(c(X-1, Y-1)), o(c(X, Y)) -> (false);
        noWall(c(X-1, Y-1)), h(c(X, Y)) -> (CN = c(X, Y + 1));
        \+noWall(c(X-1, Y-1)) -> (false)
    );
    A = up_left -> (
        noWall(c(X-1, Y+1)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X-1, Y+1), A, CN));
        noWall((X-1, Y+1)), o(c(X, Y)) -> (false);
        noWall((X-1, Y+1)), h(c(X, Y)) -> (CN = c(X, Y + 1));
        \+noWall((X-1, Y+1)) -> (false)
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
o(1, 2).
t(1, 3).

consult(input).
main :-
    randomSearch(10000, Res, Path, 50, FinalMin, FinalPath),
    write(FinalMin),
    nl,
    write(FinalPath).

randomSearch(Min, Res, Path, 0, FinalMin, FinalPath) :-
    FinalMin is Min,
    FinalPath = Path.

randomSearch(Min, Res, Path, Count, FinalMin, FinalPath) :- 
    randomMove(ball(c(0, 0), s0), 0, cont, FinalCount, FinalSate, [], FinalStack),
    Count0 is Count - 1,
    (
        (FinalCount < Min, FinalSate = td -> randomSearch(FinalCount, FinalSate, FinalStack, Count0, FinalMin, FinalPath));
        (FinalCount >= Min -> randomSearch(Min, Res, Path, Count0, FinalMin, FinalPath));
        (FinalSate \= td -> randomSearch(Min, Res, Path, Count0, FinalMin, FinalPath))
    ).

randomMove(ball(C, S), Count, Res, FinalCount, FinalSate, Stack, FinalStack) :-
    random(0, 4, Choose),
    ((Choose is 0 -> (
        Res = cont,
        \+o(C), \+t(C), \+h(C),
        randomPass(C, A),
        (toss(C, A, CN) -> randomMove(ball(CN, do(A, S)), Count0, cont, FinalCount, FinalSate, ["p" | Stack], FinalStack));
        (\+toss(C, A, CN) -> randomMove(ball(C, S), Count, lost,  FinalCount, FinalSate, [C | Stack], FinalStack))
    ));
    ((
        Res = cont,
        \+o(C), \+t(C), \+h(C),
        randomDirection(C, A),
        ball(C0, do(A, S)),
        Count0 is Count + 1,
        randomMove(ball(C0, do(A, S)), Count0, cont, FinalCount, FinalSate, [C | Stack], FinalStack)
    );
    (
        Res = cont,
        o(C),
        randomMove(ball(C0, S), Count, orc,  FinalCount, FinalSate, [C | Stack], FinalStack)
    );
    (
        Res = cont,
        h(C),
        randomDirection(C, A),
        ball(C0, do(A, S)),
        Count0 is Count,
        randomMove(ball(C0, do(A, S)), Count0, cont,  FinalCount, FinalSate, [C | Stack], FinalStack)
    );
    (
        Res = cont,
        t(C),
        randomMove(ball(C, S), Count, td,  FinalCount, FinalSate, [C | Stack], FinalStack)
    );
    (
        Res = orc,
        FinalCount is Count,
        FinalSate = orc,
        FinalStack = Stack
    );
    (
        Res = td,
        FinalCount is Count,
        FinalSate = td,
        FinalStack = Stack
    );(
        Res = lost,
        FinalCount is Count,
        FinalSate = lost,
        FinalStack = Stack
    ))).