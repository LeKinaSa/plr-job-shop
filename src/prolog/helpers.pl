
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

% list_element(+Index, +List, -Element)
list_element(Index, [Element | _], Element) :-
    Index #= 1.
list_element(Index, [_ | List], Element) :-
    Index #> 1,
    NextIndex #= Index - 1,
    list_element(NextIndex, List, Element).
