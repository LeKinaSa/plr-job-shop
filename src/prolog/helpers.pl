
:- use_module(library(clpfd)).

% start_trace
start_trace :-
    prolog_flag(debugging, _, trace).

% stop_trace
stop_trace :-
    prolog_flag(debugging, _, off).

% select_second_elements(+ListOfTuples, -ListOfSecondElements)
select_second_elements([], []).
select_second_elements([_-SecondElement | ListOfTuples], [SecondElement | ListOfSecondElements]) :-
    select_second_elements(ListOfTuples, ListOfSecondElements).

% pair_element(+Index, +List, -PairElement)
pair_element(Index, List, First-Second) :-
    divide_list(List,  FirstElementList, SecondElementList),
    element(Index,  FirstElementList,  First),
    element(Index, SecondElementList, Second).

% divide_list(+List, -FirstElementList, -SecondElementList)
divide_list([], [], []).
divide_list([First-Second | List], [First | FirstElementList], [Second | SecondElementList]) :-
    divide_list(List, FirstElementList, SecondElementList).
