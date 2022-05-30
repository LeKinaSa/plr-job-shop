
:- ensure_loaded('helpers.pl').

% print(+Tasks, +Starts, +Ends, +Chosen, +Horizon, +ObjectiveFunctionValue)
print(Tasks, Start, End, Chosen, Horizon, ObjFunc) :-
    divide_tasks(Tasks, Chosen, JobIds, Machines, Interval),
    write('Jobs     '), write(JobIds  ), nl,
    write('Machines '), write(Machines), nl,
    write('Start    '), write(Start   ), nl,
    write('Interval '), write(Interval), nl,
    write('End      '), write(End     ), nl,
    write('Chosen   '), write(Chosen  ), nl,
    write('Horizon  '), write(Horizon ), nl,
    write('ObjFunc  '), write(ObjFunc ), nl.

% divide_tasks(+Tasks, +Chosen, -JobIds, -Machines, -Intervals) ; Tasks = [JobId-MinStart-MaxEnd-Task]
divide_tasks([], [], [], [], []).
divide_tasks([JobId-_-_-Task | Tasks], [ChosenAltTask | Chosen], [JobId | JobIds], [Machine | Machines], [Interval | Intervals]) :-
    pair_element(ChosenAltTask, Task, Machine-Interval),
    divide_tasks(Tasks, Chosen, JobIds, Machines, Intervals).

% reset_timer → Clean / Reset Statistics Timer
reset_timer :-
    statistics(total_runtime, _).

% print_time(+Msg, -Time) → Print Statistics Time with a Message
print_time(Msg, TimeInSeconds) :-
    statistics(total_runtime, [_, TimeInMilliseconds]),
    TimeInSeconds is ((TimeInMilliseconds // 10) * 10) / 1000,
    write(Msg), write(TimeInSeconds), write('s'), nl.
