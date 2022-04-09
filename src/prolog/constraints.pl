
:-use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%   Task Duration    %%%%%%%%%%%%%%%%%%%%

% task_duration(+Tasks, +Start, +End)
task_duration([], [], []).
task_duration([_-Task | Tasks], [Start | Starts], [End | Ends]) :-
    task_duration(Task, Start-End),
    task_duration(Tasks, Starts, Ends).

% task_duration(Machine-Duration, Start-End)
task_duration(_-Interval, Start-End) :-
    Interval #= End - Start.

%%%%%%%%%%%%%%%%%%%% 1 Task per Machine %%%%%%%%%%%%%%%%%%%%

% only_one_task_per_machine_at_a_time(+Tasks, +Start, +End, +Chosen)
only_one_task_per_machine_at_a_time([], [], [], []).
only_one_task_per_machine_at_a_time([_ | Tasks], [_ | Starts], [_ | Ends], [YesOrNo | Chosen]) :-
    YesOrNo #= 0,
    only_one_task_per_machine_at_a_time(Tasks, Starts, Ends, Chosen).
only_one_task_per_machine_at_a_time([_-(Machine-_) | Tasks], [Start | Starts], [End | Ends], [YesOrNo | Chosen]) :-
    YesOrNo #= 1, % Task was Chosen, so no other task can occur in the same machine at the same time
    no_other_task(Machine, Start-End, Starts, Ends, Chosen),
    only_one_task_per_machine_at_a_time(Tasks, Starts, Ends, Chosen).

% no_other_task(+Machine, +Start-End, +Tasks, +Starts, +Ends, +Chosen)
no_other_task(TaskMachine, Interval, [_ | Tasks], [_ | Starts], [_ | Ends], [YesOrNo | Chosen]) :-
    YesOrNo #= 0, % Task was not Chosen
    no_other_task(TaskMachine, Interval, Tasks, Starts, Ends, Chosen).
no_other_task(TaskMachine, Interval, [_-(Machine-_) | Tasks], [_ | Starts], [_ | Ends], [YesOrNo | Chosen]) :-
    YesOrNo #= 1,
    TaskMachine #\= Machine, % Task is in a different machine
    no_other_task(TaskMachine, Interval, Tasks, Starts, Ends, Chosen).
no_other_task(TaskMachine, StartTask-EndTask, [_-(Machine-_) | Tasks], [_ | Starts], [End | Ends], [YesOrNo | Chosen]) :-
    YesOrNo #= 1,
    TaskMachine #= Machine,
    End =< StartTask, % The other Task was before this one
    no_other_task(TaskMachine, StartTask-EndTask, Tasks, Starts, Ends, Chosen).
no_other_task(TaskMachine, StartTask-EndTask, [_-(Machine-_) | Tasks], [Start | Starts], [_ | Ends], [YesOrNo | Chosen]) :-
    YesOrNo #= 1,
    TaskMachine #= Machine,
    EndTask =< Start, % The other Task was after this one
    no_other_task(TaskMachine, StartTask-EndTask, Tasks, Starts, Ends, Chosen).

%%%%%%%%%%%%%%%%%%%% 1 Alternative Task %%%%%%%%%%%%%%%%%%%%

% only_one_chosen_alternative_task(+Tasks, +Chosen)
only_one_chosen_alternative_task(_, _).
