%HELPERS

%Counts number of elements in list
countList([],0). 
countList([_|Tail], N) :- countList(Tail, N1), N is N1 + 1.

%Check if item is on the list or not
on_list(Item,[Item|Rest]).
on_list(Item,[DisregardHead|Tail]):-
    \+ Tail = [],
    on_list(Item,Tail).

%Count the number of similar elements in two lists
colisions(L1, L2, Count, FCount) :-
    (L1 = [], FCount is Count);
    (
        L1 = [H|T],
        (
            (on_list(H, L2), CountN is Count + 1, colisions(T, L2, CountN, FCount));
            (\+on_list(H, L2), CountN is Count, colisions(T, L2, CountN, FCount))
        )
    ).

%Join two sets (no repetitions)
join(L1, L2, LF) :-
    (L1 = [], LF = L2);
    (
        L1 = [H|T],
        (
            \+on_list(H, L2) -> (LN = [H|L2], join(T, LN, LF));
            on_list(H, L2) -> (join(T, L2, LF))
        )
    ).

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

ball(c(0, 0), s0).
ball(C, do(A, S)) :- 
    ball(C0, S),
    (
        (A = up, cellAbove(C0, C), noWall(C));
        (A = down, cellBelow(C0, C), noWall(C));
        (A = left, cellLeft(C0, C), noWall(C));
        (A = right, cellRight(C0, C), noWall(C))
    ).

t(x0, y0).
h(x0, y0).
o(x0, y0).

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

isAdjacent(C, CN) :-
    cellRight(C, CN);
    cellBelow(C, CN);
    cellAbove(C, CN);
    cellLeft(C, CN).

%------------------------------------Random------------------------------------
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
        YN is Y+1, 
        ((noWall(c(X, YN)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X, YN), A, CN)));
        (noWall(c(X, YN)), o(c(X, Y)) -> (false));
        (noWall(c(X, YN)), h(c(X, Y)) -> (CN = c(X, Y)));
        (\+noWall(c(X, YN)) -> (false)))
    );
    A = down -> (
        YN is Y - 1,
        ((noWall(c(X, YN)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(X, YN), A, CN)));
        (noWall(c(X, YN)), o(c(X, Y)) -> (false));
        (noWall(c(X, YN)), h(c(X, Y)) -> (CN = c(X, Y)));
        (\+noWall(c(X, YN)) -> (false)))
    );
    A = left -> (
        XN is X - 1,
        ((noWall(c(XN, Y)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(XN, Y), A, CN)));
        (noWall(c(XN, Y)), o(c(X, Y)) -> (false));
        (noWall(c(XN, Y)), h(c(X, Y)) -> (CN = c(X, Y)));
        (\+noWall(c(XN, Y)) -> (false)))
    );
    A = right -> (
        XN is X + 1,
        ((noWall(c(XN, Y)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(XN, Y), A, CN)));
        (noWall(c(XN, Y)), o(c(X, Y)) -> (false));
        (noWall(c(XN, Y)), h(c(X, Y)) -> (CN = c(X, Y)));
        (\+noWall(c(XN, Y)) -> (false)))
    );
    A = up_right -> (
        XN is X + 1,
        YN is Y + 1,
        ((noWall(c(XN, YN)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(XN, YN), A, CN)));
        (noWall(c(XN, YN)), o(c(X, Y)) -> (false));
        (noWall(c(XN, YN)), h(c(X, Y)) -> (CN = c(X, Y)));
        (\+noWall(c(XN, YN)) -> (false)))
    );
    A = down_right -> (
        XN is X + 1,
        YN is Y - 1,
        ((noWall(c(XN, YN)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(XN, YN), A, CN)));
        (noWall(c(XN, YN)), o(c(X, Y)) -> (false));
        (noWall(c(XN, YN)), h(c(X, Y)) -> (CN = c(X, Y)));
        (\+noWall(c(XN, YN)) -> (false)))
    );
    A = down_left -> (
        XN is X - 1,
        YN is Y - 1,
        ((noWall(c(XN, YN)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(XN, YN), A, CN)));
        (noWall(c(XN, YN)), o(c(X, Y)) -> (false));
        (noWall(c(XN, YN)), h(c(X, Y)) -> (CN = c(X, Y)));
        (\+noWall(c(XN, YN)) -> (false)))
    );
    A = up_left -> (
        XN is X - 1,
        YN is Y + 1,
        ((noWall(c(XN, YN)), \+o(c(X, Y)), \+h(c(X, Y)) -> (toss(c(XN, YN), A, CN)));
        (noWall((XN, YN)), o(c(X, Y)) -> (false));
        (noWall((XN, YN)), h(c(X, Y)) -> (CN = c(X, Y)));
        (\+noWall((XN, YN)) -> (false)))
    ).

simulatePath(c(X, Y), S, A, c(XN, YN), SF) :- 
    A = up -> (
        (Y \= YN,
        Y0 is Y + 1,
        simulatePath(c(X, Y0), do(up, S), A, c(XN, YN), SF));
        (Y = YN, SF = S)
    );
    A = down -> (
        (Y \= YN,
        Y0 is Y - 1,
        simulatePath(c(X, Y0), do(down, S), A, c(XN, YN), SF));
        (Y = YN, SF = S)
    );
    A = left -> (
        (X \= XN,
        X0 is X - 1,
        simulatePath(c(X0, Y), do(left, S), A, c(XN, YN), SF));
        (X = XN, SF = S)
    );
    A = right -> (
        (X \= XN,
        X0 is X + 1,
        simulatePath(c(X0, Y), do(right, S), A, c(XN, YN), SF));
        (X = XN, SF = S)
    );
    A = up_right -> (
        (X \= XN, Y \= YN,
        X0 is X + 1, Y0 is Y + 1,
        simulatePath(c(X0, Y0), do(right, do(up, S)), A, c(XN, YN), SF));
        (X = XN, Y = YN, SF = S)
    );
    A = down_right -> (
        (X \= XN, Y \= YN,
        X0 is X + 1, Y0 is Y - 1,
        simulatePath(c(X0, Y0), do(right, do(down, S)), A, c(XN, YN), SF));
        (X = XN, Y = YN, SF = S)
    );
    A = down_left -> (
        (X \= XN, Y \= YN,
        X0 is X - 1, Y0 is Y - 1,
        simulatePath(c(X0, Y0), do(left, do(down, S)), A, c(XN, YN), SF));
        (X = XN, Y = YN, SF = S)
    );
    A = up_left -> (
        (X \= XN, Y \= YN,
        X0 is X - 1, Y0 is Y + 1,
        simulatePath(c(X0, Y0), do(left, do(up, S)), A, c(XN, YN), SF));
        (X = XN, Y = YN, SF = S)
    ). 

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
    ((
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
    );
    (
        Res = cont,
        \+o(C), \+t(C), \+h(C),
        (
            Choose is 0 -> (
                randomPass(C, A),
                ((toss(C, A, CN) -> simulatePath(C, S, A, CN, SF), Count0 is Count + 2, randomMove(ball(CN, SF), Count0, cont, FinalCount, FinalSate, ['p' | Stack], FinalStack));
                (\+toss(C, A, CN) -> randomMove(ball(C, S), Count, lost,  FinalCount, FinalSate, [C | Stack], FinalStack)))
            );
            (
                randomDirection(C, A),
                ball(C0, do(A, S)),
                Count0 is Count + 1,
                randomMove(ball(C0, do(A, S)), Count0, cont, FinalCount, FinalSate, [C | Stack], FinalStack)
            )
        )
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
     )).

%------------------------------------BTS------------------------------------
backtrackSearch(c(X, Y), L, Count, FL, FCount) :-
    \+on_list(c(X, Y), L),
    ((
        t(c(X, Y)),
        FL = [c(X, Y) | L],
        FCount = Count, !
    );
    (
        cellAbove(c(X, Y), CN),
        t(CN),
        CountN is Count + 1,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)
    );
    (
        cellBelow(c(X, Y), CN),
        t(CN),
        CountN is Count + 1,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)
    );
    (
        cellLeft(c(X, Y), CN),
        t(CN),
        CountN is Count + 1,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)
    );
    (
        cellRight(c(X, Y), CN),
        t(CN),
        CountN is Count + 1,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)
    );
    (
        toss(c(X, Y + 1), up, CN),
        CountN is Count + 1,
        backtrackSearch(CN, ['p', c(X,Y)|L], CountN, FL, FCount)
    );
    (
        toss(c(X, Y - 1), down, CN),
        CountN is Count + 1,
        backtrackSearch(CN, ['p', c(X,Y)|L], CountN, FL, FCount)
    );
    (
        toss(c(X-1, Y), left, CN),
        CountN is Count + 1,
        backtrackSearch(CN, ['p', c(X,Y)|L], CountN, FL, FCount)
    );
    (
        toss(c(X+1, Y), right, CN),
        CountN is Count + 1,
        backtrackSearch(CN, ['p', c(X,Y)|L], CountN, FL, FCount)
    );
    (
        toss(c(X-1, Y+1), up_left, CN),
        CountN is Count + 1,
        backtrackSearch(CN, ['p', c(X,Y)|L], CountN, FL, FCount)
    );
    (
        toss(c(X-1, Y-1), down_left, CN),
        CountN is Count + 1,
        backtrackSearch(CN, ['p', c(X,Y)|L], CountN, FL, FCount)
    );
    (
        toss(c(X+1, Y+1), up_right, CN),
        CountN is Count + 1,
        backtrackSearch(CN, ['p', c(X,Y)|L], CountN, FL, FCount)
    );
    (
        toss(c(X+1, Y-1), down_right, CN),
        CountN is Count + 1,
        backtrackSearch(CN, ['p', c(X,Y)|L], CountN, FL, FCount)
    );
    (
        cellAbove(c(X, Y), CN),
        h(CN),
        CountN is Count,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)
    );
    (
        cellBelow(c(X, Y), CN),
        h(CN),
        CountN is Count,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)
    );
    (
        cellLeft(c(X, Y), CN),
        h(CN),
        CountN is Count,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)
    );
    (
        cellRight(c(X, Y), CN),
        h(CN),
        CountN is Count,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)
    );
    (
        cellAbove(c(X, Y), CN),
        noWall(CN),
        \+o(CN),
        CountN is Count + 1,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)
    );
    (
        cellLeft(c(X, Y), CN),
        noWall(CN),
        \+o(CN),
        CountN is Count + 1,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)  
    );
    (
        cellRight(c(X, Y), CN),
        noWall(CN),
        \+o(CN),
        CountN is Count + 1,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)  
    );
    (
        cellBelow(c(X, Y), CN),
        noWall(CN),
        \+o(CN),
        CountN is Count + 1,
        backtrackSearch(CN, [c(X, Y)|L], CountN, FL, FCount)  
    )).
    
%------------------------------------A_Star------------------------------------
listAdjacent(C, List, Count, FList) :-
    Count is 0 -> (
        C = c(X, Y),
        cellAbove(C, CA),
        (
            noWall(CA) -> (Res0 = [CA], listAdjacent(C, Res0, 1, FList));
            \+noWall(CA) -> (listAdjacent(C, List, 1, FList))
        )    
    );
    Count is 1 -> (
        C = c(X, Y),
        cellBelow(C, CB),
        (
            noWall(CB) -> (Res0 = [CB|List], listAdjacent(C, Res0, 2, FList));
            \+noWall(CB) -> (listAdjacent(C, List, 2, FList))
        ) 
    );
    Count is 2 -> (
        C = c(X, Y),
        cellRight(C, CR),
        (
            noWall(CR) -> (Res0 = [CR|List], listAdjacent(C, Res0, 3, FList));
            \+noWall(CR) -> (listAdjacent(C, List, 3, FList))
        ) 
    );
    Count is 3 -> (
        C = c(X, Y),
        cellLeft(C, CL),
        (
            noWall(CL) -> (Res0 = [CL|List], FList = Res0);
            \+noWall(CL) -> (FList = List)
        ) 
    ).

chooseBestRec(C, Adj, Discovered, CF, Max, CMin) :-
    (
        Adj = [],
        listAdjacent(C, [], 0, AdjN),
        colisions(AdjN, Discovered, 0, Coll),
        countList(AdjN, AdjLen),
        (
            (AdjLen - Coll > Max), \+o(C) -> (CF = C);
            (CF = CMin)
        )
    );
    (
        listAdjacent(C, [], 0, AdjN),
        colisions(AdjN, Discovered, 0, Coll),
        countList(AdjN, AdjLen),
        (
            (AdjLen - Coll > Max), \+o(C) -> CollN is AdjLen - Coll, (Adj = [CN|T], chooseBestRec(CN, T, Discovered, CF, CollN, C));
            (Adj = [CN|T], chooseBestRec(CN, T, Discovered, CF, Max, CMin))
        )
    ).

chooseBest(C, Discovered, CN) :- 
    listAdjacent(C, [], 0, Adj),
    Adj = [H|T],
    chooseBestRec(H, T, Discovered, CF, 0, C),
    CN = CF.

aStarSearch(c(X, Y), L, Count, Discovered, FL, FCount) :-
    \+on_list(c(X, Y), L),
    listAdjacent(c(X, Y), [], 0, Adj),
    join(Discovered, Adj, DiscoveredN),
    ((
        t(c(X, Y)),
        FL = [c(X, Y) | L],
        FCount = Count, !
    );
    (
        cellAbove(c(X, Y), CN),
        t(CN),
        CountN is Count + 1,
        aStarSearch(CN, [c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        cellBelow(c(X, Y), CN),
        t(CN),
        CountN is Count + 1,
        aStarSearch(CN, [c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        cellLeft(c(X, Y), CN),
        t(CN),
        CountN is Count + 1,
        aStarSearch(CN, [c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        cellRight(c(X, Y), CN),
        t(CN),
        CountN is Count + 1,
        aStarSearch(CN, [c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        YN is Y + 1,
        toss(c(X, YN), up, CN),
        CountN is Count + 1,
        aStarSearch(CN, ['p', c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        YN is Y - 1,
        toss(c(X, YN), down, CN),
        CountN is Count + 1,
        aStarSearch(CN, ['p', c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        XN is X - 1,
        toss(c(XN, Y), left, CN),
        CountN is Count + 1,
        aStarSearch(CN, ['p', c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        XN is X + 1,
        toss(c(XN, Y), right, CN),
        CountN is Count + 1,
        aStarSearch(CN, ['p', c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        XN is X - 1,
        YN is Y + 1,
        toss(c(XN, YN), up_left, CN),
        CountN is Count + 1,
        aStarSearch(CN, ['p', c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        XN is X - 1,
        YN is Y - 1,
        toss(c(XN, YN), down_left, CN),
        CountN is Count + 1,
        aStarSearch(CN, ['p', c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        XN is X + 1,
        YN is Y + 1,
        toss(c(XN, YN), up_right, CN),
        CountN is Count + 1,
        aStarSearch(CN, ['p', c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        XN is X + 1,
        YN is Y - 1,
        toss(c(XN, YN), down_right, CN),
        CountN is Count + 1,
        aStarSearch(CN, ['p', c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        cellAbove(c(X, Y), CN),
        h(CN),
        CountN is Count,
        aStarSearch(CN, [c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        cellBelow(c(X, Y), CN),
        h(CN),
        CountN is Count,
        aStarSearch(CN, [c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        cellLeft(c(X, Y), CN),
        h(CN),
        CountN is Count,
        aStarSearch(CN, [c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        cellRight(c(X, Y), CN),
        h(CN),
        CountN is Count,
        aStarSearch(CN, [c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    );
    (
        chooseBest(c(X, Y), DiscoveredN, CN),
        CountN is Count + 1,
        aStarSearch(CN, [c(X, Y)|L], CountN, DiscoveredN, FL, FCount)
    )).


main :-
    consult(input),
    statistics(runtime,[StartRand|_]),
    randomSearch(1000000, Res, Path, 100, FinalMin, FinalPath),
    statistics(runtime,[StopRand|_]),
    RuntimeRand is StopRand - StartRand,
    nl, nl,
    write('Random Search Took '), write(RuntimeRand), write('ms.'),
    nl,
    write(FinalMin),
    nl,
    write(FinalPath), nl, nl,
    statistics(runtime,[StartBTS|_]),
    backtrackSearch(c(0, 0), [], 0, FL, FCount),
    statistics(runtime,[StopBTS|_]),
    RuntimeBTS is StopBTS - StartBTS,
    write('BTS Took '), write(RuntimeBTS), write('ms.'),
    nl,
    write(FCount),
    nl,
    write(FL), nl, nl,
    statistics(runtime,[StartAStar|_]),
    aStarSearch(c(0, 0), [], 0, [c(0, 0)], Fin, FinC),
    statistics(runtime,[StopAStar|_]),
    RuntimeAStar is StopAStar - StartAStar,
    write('A-Star Took '), write(RuntimeAStar), write('ms.'),
    nl,
    write(FinC),
    nl,
    write(Fin), nl, nl.