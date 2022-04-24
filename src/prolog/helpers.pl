% start_trace
start_trace :-
    prolog_flag(debugging, _, trace).

% stop_trace
stop_trace :-
    prolog_flag(debugging, _, off).

% list_element(+Index, +List, -Element)
list_element(1, [Element | _], Element).
list_element(Index, [_, List], Element) :-
    Index #> 1,
    NextIndex is Index - 1,
    list_element(NextIndex, List, Element).
