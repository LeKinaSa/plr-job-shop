
:-use_module(library(clpfd)).

% infinite_machine(-InfiniteMachine)
infinite_machine(0).

%%%%%%%%%%%%%%%%%%%%   Task Duration    %%%%%%%%%%%%%%%%%%%%

% task_duration(+Tasks, +ChosenAltTasks, +Starts, +Ends)
task_duration([], [], [], []).
task_duration([_-Task | Tasks], [ChosenAltTask | Chosen], [Start | Starts], [End | Ends]) :-
    verify_task_duration(Task, ChosenAltTask, Start, End),
    task_duration(Tasks, Chosen, Starts, Ends).

% verify_task_duration(+Task, +ChosenAltTask, +Start, +End)
verify_task_duration(Task, Chosen, Start, End) :-
    list_element(Chosen, Task, _-Duration),
    End #= Start + Duration.

%%%%%%%%%%%%%%%%%%%% 1 Task per Machine %%%%%%%%%%%%%%%%%%%%

% only_one_task_per_machine_at_a_time(+Tasks, +ChosenAltTasks, +Starts, +Ends)
only_one_task_per_machine_at_a_time(_, _, _, _).

%%%%%%%%%%%%%%%%%%%% 1 Alternative Task %%%%%%%%%%%%%%%%%%%%

% only_one_chosen_alternative_task(+Tasks, +Chosen)
only_one_chosen_alternative_task([], []).
only_one_chosen_alternative_task([_-Task | Tasks], [ChosenAltTask | Chosen]) :-
    length(Task, NumberOfAltTasks),
    ChosenAltTask in 1..NumberOfAltTasks,
    only_one_chosen_alternative_task(Tasks, Chosen).
