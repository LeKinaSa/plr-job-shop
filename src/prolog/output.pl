
:- ensure_loaded('helpers.pl').

% print(+Tasks, +Starts, +Ends, +Chosen, +Horizon, +ObjectiveFunctionValue)
print(Tasks, Start, End, Chosen, Horizon, ObjFunc) :-
    divide_tasks(Tasks, Chosen, JobIds, TaskIds, Machines, Interval),
    write('Jobs     '), write(JobIds  ), nl,
    write('Tasks    '), write(TaskIds ), nl,
    write('Machines '), write(Machines), nl,
    write('Start    '), write(Start   ), nl,
    write('Interval '), write(Interval), nl,
    write('End      '), write(End     ), nl,
    write('Chosen   '), write(Chosen  ), nl,
    write('Horizon  '), write(Horizon ), nl,
    write('ObjFunc  '), write(ObjFunc ), nl.

% divide_tasks(+Tasks, +Chosen, -JobIds, -TaskIds, -Machines, -Intervals)
divide_tasks([], [], [], [], [], []).
divide_tasks([(JobId-TaskId)-Task | Tasks], [ChosenAltTask | Chosen], [JobId | JobIds], [TaskId | TaskIds], [Machine | Machines], [Interval | Intervals]) :-
    pair_element(ChosenAltTask, Task, Machine-Interval),
    divide_tasks(Tasks, Chosen, JobIds, TaskIds, Machines, Intervals).

% reset_timer → Clean / Reset Statistics Timer
reset_timer :-
    statistics(total_runtime, _).

% print_time(+Msg) → Print Statistics Time with a Message
print_time(Msg) :-
    statistics(total_runtime, [_, TimeInMilliseconds]),
    TimeInSeconds is ((TimeInMilliseconds // 10) * 10) / 1000,
    write(Msg), write(TimeInSeconds), write('s'), nl.
