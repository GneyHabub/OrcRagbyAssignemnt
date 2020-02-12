parent(mom, me).
parent(dad, me).
parent(mom, sister).
parent(dad, sister).
parent(granny, mom).
parent(granny, uncle).
parent(uncle, cousine).

is_sibling(Child1, Child2) :-
    parent(Parent1, Child1),
    parent(Parent2, Child2),
    parent(Parent1, Child2),
    parent(Parent2, Child1).

is_cousine(Child1, Child2) :-
    is_sibling(Parent1, Parent2),
    parent(Parent1, Child1),
    parent(Parent2, Child2).