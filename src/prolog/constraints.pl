
:- use_module(library(clpfd)).

% infinite_machine(-InfiniteMachineId, -InfiniteMachineCapacity)
infinite_machine(0, 1000).

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
    pair_element(Chosen, Task, _-Duration),
    End #= Start + Duration.

%%%%%%%%%%%%%%%%%%%% 1 Task per Machine %%%%%%%%%%%%%%%%%%%%

% only_one_task_per_machine_at_a_time(+Tasks, +ChosenAltTasks, +Starts, +Ends)
only_one_task_per_machine_at_a_time(Tasks, ChosenAltTasks, Starts, Ends) :-
    n_machines(NumberOfMachines),

    get_cumulative_tasks(Tasks, ChosenAltTasks, Starts, Ends, CumulativeTasks),
    get_machines(NumberOfMachines, Machines),

    cumulatives(CumulativeTasks, Machines, [bound(upper)]).

% get_machines(+NumberOfMachines, -Machines) → Machines = [machine(MachineId, Limit)]
get_machines(0, [machine(InfiniteMachineId, InfiniteMachineCapacity)]) :-
    infinite_machine(InfiniteMachineId, InfiniteMachineCapacity), !.
get_machines(MachineId, [machine(MachineId, 1) | Machines]) :-
    NextMachineId #= MachineId - 1,
    get_machines(NextMachineId, Machines).

% get_cumulative_tasks(+Tasks, +ChosenAltTasks, +Starts, +Ends, -CumulativeTasks)
get_cumulative_tasks([], [], [], [], []).
get_cumulative_tasks([Task | Tasks], [ChosenAltTask | ChosenAltTasks], [Start | Starts], [End | Ends], [CumulativeTask | CumulativeTasks]) :-
    get_cumulative_task(Task, ChosenAltTask, Start, End, CumulativeTask),
    get_cumulative_tasks(Tasks, ChosenAltTasks, Starts, Ends, CumulativeTasks).

% get_cumulative_task(+Task, +ChosenAltTask, +Start, +End, -CumulativeTask)
%   CumulativeTask = [task(Start_i, Duration_i, End_i, Resource_Usage, MachineId)]
get_cumulative_task(_-AltTasks, ChosenAltTask, Start, End, task(Start, Duration, End, 1, Machine)) :-
    pair_element(ChosenAltTask, AltTasks, Machine-Duration).

%%%%%%%%%%%%%%%%%%%% Task Precedence %%%%%%%%%%%%%%%%%%%%

% task_precedence(+Tasks, +Starts, +Ends)
task_precedence([_], [_], [_]).
task_precedence([Task1, Task2 | Tasks], [Start1, Start2 | Starts], [End1, End2 | Ends]) :-
    precedence(Task1, Task2, Start1-Start2, End1-End2),
    task_precedence([Task2 | Tasks], [Start2 | Starts], [End2 | Ends]).

% precedence(+Task1, +Task2, +Starts, +Ends)
%   Assumes that the tasks are ordered in JobId-TaskId
precedence(_, (_-0)-_, _, _).
precedence(_, (_-TaskId)-_, _-Start2, End1-_) :-
    TaskId > 0,
    Start2 #>= End1.
