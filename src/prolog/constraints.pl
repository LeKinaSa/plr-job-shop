
:- use_module(library(clpfd)).

% infinite_machine(-InfiniteMachine)
infinite_machine(0).

%%%%%%%%%%%%%%%%%%%% 1 Alternative Task %%%%%%%%%%%%%%%%%%%%

% only_one_chosen_alternative_task(+Tasks, +Chosen)
only_one_chosen_alternative_task([], []).
only_one_chosen_alternative_task([_-Task | Tasks], [ChosenAltTask | Chosen]) :-
    length(Task, NumberOfAltTasks),
    ChosenAltTask in 1..NumberOfAltTasks,
    only_one_chosen_alternative_task(Tasks, Chosen).

%%%%%%%%%%%%%%%%%%%%   Task Duration    %%%%%%%%%%%%%%%%%%%%

% task_duration(+Tasks, +ChosenAltTasks, +Starts, +Ends)
task_duration([], [], [], []).
task_duration([_-Task | Tasks], [ChosenAltTask | Chosen], [Start | Starts], [End | Ends]) :-
    verify_task_duration(Task, ChosenAltTask, Start, End),
    task_duration(Tasks, Chosen, Starts, Ends).

% verify_task_duration(+Task, +ChosenAltTask, +Start, +End)
verify_task_duration(Task, Chosen, Start, End) :-
    list_element(Chosen, Task, _-Duration), % TODO: this is initializing the Chosen vector
    End #= Start + Duration.

%%%%%%%%%%%%%%%%%%%% 1 Task per Machine %%%%%%%%%%%%%%%%%%%%

% only_one_task_per_machine_at_a_time(+Tasks, +ChosenAltTasks, +Starts, +Ends)
only_one_task_per_machine_at_a_time(Tasks, ChosenAltTasks, Starts, Ends) :-
    get_n_machines(Machines),

    get_cumulative_tasks(Tasks, ChosenAltTasks, Starts, Ends, CumulativeTasks),
    get_capacities(Machines, Capacities),
    % get_precedences(Tasks, Precedences),% TODO: uncomment

    multi_cumulative(CumulativeTasks, Capacities, []). % precedences(Precedences) % TODO: add to options

% get_capacities(+Machines, -Capacities)
get_capacities(0, []) :- !.
get_capacities(N, [cumulative(1) | Capacities]) :-
    NextN #= N - 1,
    get_capacities(NextN, Capacities).

% get_precedences(+Tasks, -Precedences) → Precedences = [Task1-Task2], Task1 before Task2
get_precedences(_, []). % TODO

% get_cumulative_tasks(+Tasks, +ChosenAltTasks, +Starts, +Ends, -CumulativeTasks)
get_cumulative_tasks([], [], [], [], []).
get_cumulative_tasks([Task | Tasks], [ChosenAltTask | ChosenAltTasks], [Start | Starts], [End | Ends], [CumulativeTask | CumulativeTasks]) :-
    get_cumulative_task(Task, ChosenAltTask, Start, End, CumulativeTask),
    get_cumulative_tasks(Tasks, ChosenAltTasks, Starts, Ends, CumulativeTasks).

% get_cumulative_task(+Task, +ChosenAltTask, +Start, +End, -CumulativeTask)
%   CumulativeTask = task(Start_i, Duration_i, End_i, Machine_i, Task_id_i)
get_cumulative_task((JobId-TaskId)-AltTasks, ChosenAltTask, Start, End, task(Start, Duration, End, Machine, Identifier)) :-
    list_element(ChosenAltTask, AltTasks, Machine-Duration), % TODO: this is initializing the Chosen vector
    Identifier #= JobId * 10 + TaskId.
