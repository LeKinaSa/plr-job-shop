
:-use_module(library(clpfd)).

% max(+Value1, +Value2, -Max)
max(Value1, Value2, Value1) :-
    Value1 #>= Value2.
max(Value1, Value2, Value2) :-
    Value1 #< Value2.

% max_list(+List, +KnownMax, -Max)
max_list([], Max, Max).
max_list([Value | List], KnownMax, Max) :-
    Value #> KnownMax,
    max_list(List, Value, Max).
max_list([Value | List], KnownMax, Max) :-
    Value #=< KnownMax,
    max_list(List, KnownMax, Max).
