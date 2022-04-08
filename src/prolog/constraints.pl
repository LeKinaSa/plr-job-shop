
:-use_module(library(clpfd)).

% task_duration(+Tasks, +Start, +End)
task_duration([], [], []).
task_duration([_-Task | Tasks], [Start | Starts], [End | Ends]) :-
    task_duration(Task, Start-End),
    task_duration(Tasks, Starts, Ends).

task_duration(_-Interval, Start-End) :-
    Interval #= End - Start.

% only_one_task_per_machine_at_a_time(+Tasks, +Start, +End, +Chosen)
only_one_task_per_machine_at_a_time([], [], [], []).
only_one_task_per_machine_at_a_time(_, _, _, _).

% only_one_chosen_alternative_task(+Tasks, +Chosen)
only_one_chosen_alternative_task(_, _).
