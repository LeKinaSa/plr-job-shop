
% max(+Value1, +Value2, -Max)
max(Value1, Value2, Value1) :-
    Value1 >= Value2.
max(Value1, Value2, Value2) :-
    Value1 < Value2.
