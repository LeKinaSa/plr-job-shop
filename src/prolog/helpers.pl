
:- use_module(library(clpfd)).

% start_trace
start_trace :-
    prolog_flag(debugging, _, trace).

% stop_trace
stop_trace :-
    prolog_flag(debugging, _, off).

% pair_element(+Index, +List, -PairElement)
pair_element(Index, List, First-Second) :-
    divide_list(List,  FirstElementList, SecondElementList),
    element(Index,  FirstElementList,  First),
    element(Index, SecondElementList, Second).

% divide_list(+List, -FirstElementList, -SecondElementList)
divide_list([], [], []).
divide_list([First-Second | List], [First | FirstElementList], [Second | SecondElementList]) :-
    divide_list(List, FirstElementList, SecondElementList).

% concat(+List, -CompleteAtom)
concat(List, CompleteAtom) :-
    concat(List, '', CompleteAtom).

% concat(+List, +TempAtom, -CompleteAtom)
concat([], CompleteAtom, CompleteAtom).
concat([Atom | List], Temp, Complete) :-
    atom(Atom),
    atom_concat(Temp, Atom, NextTemp),
    concat(List, NextTemp, Complete).
concat([Number | List], Temp, Complete) :-
    number(Number),
    atom_number(Atom, Number),
    atom_concat(Temp, Atom, NextTemp),
    concat(List, NextTemp, Complete).

% atom_number(?Atom, ?Number)
atom_number(Atom, Number) :-
    number_codes(Number, Codes),
    atom_codes(Atom, Codes).
