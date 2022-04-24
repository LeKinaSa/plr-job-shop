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
list_element(1, [Element | _], Element).
list_element(Index, [_, List], Element) :-
    Index #> 1,
    NextIndex is Index - 1,
    list_element(NextIndex, List, Element).
