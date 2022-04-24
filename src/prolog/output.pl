
% print(+Tasks, +Starts, +Ends, +Chosen, +Horizon, +ObjectiveFunctionValue)
print(Tasks, Start, End, Chosen, Horizon, ObjFunc) :-
    divide_tasks(Tasks, Machines, Interval),
    write('Machines '), write(Machines), nl,
    write('Interval '), write(Interval), nl,
    write('Start    '), write(Start   ), nl,
    write('End      '), write(End     ), nl,
    write('Chosen   '), write(Chosen  ), nl,
    write('Horizon  '), write(Horizon ), nl,
    write('ObjFunc  '), write(ObjFunc ), nl.

% divide_tasks(+Tasks, -Machines, -Intervals)
divide_tasks([], [], []).
divide_tasks([_-(M-I) | Tasks], [M | Machines], [I | Intervals]) :-
    divide_tasks(Tasks, Machines, Intervals).

% reset_timer -> Clean / Reset Statistics Timer
reset_timer :-
    statistics(total_runtime, _).

% print_time(+Msg) -> Print Statistics Time with a Message
print_time(Msg) :-
    statistics(total_runtime, [_, TimeInMilliseconds]),
    TimeInSeconds is ((TimeInMilliseconds // 10) * 10) / 1000,
    write(Msg), write(TimeInSeconds), write('s'), nl.
