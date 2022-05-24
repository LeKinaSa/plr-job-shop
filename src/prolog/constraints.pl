
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

% task_duration(+Tasks, +ChosenAltTasks, +Starts, +Ends, -Durations)
task_duration([], [], [], [], []).
task_duration([_-Task | Tasks], [ChosenAltTask | Chosen], [Start | Starts], [End | Ends], [Duration | Durations]) :-
    verify_task_duration(Task, ChosenAltTask, Start, End, Duration),
    task_duration(Tasks, Chosen, Starts, Ends, Durations).

% verify_task_duration(+Task, +ChosenAltTask, +Start, +End, -Duration)
verify_task_duration(Task, Chosen, Start, End, Duration) :-
    pair_element(Chosen, Task, _-Duration),
    End #>= Start + Duration.

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

%%%%%%%%%%%%%%%%%%%% Task Interval %%%%%%%%%%%%%%%%%%%%

% task_interval(+Tasks, +Starts, +Ends) ; Tasks = [JobId-MinStart-MaxEnd-Task]
task_interval([], [], []).
task_interval([_-MinStart-MaxEnd-_ | Tasks], [Start | Starts], [End | Ends]) :-
    MinStart #=< Start,
    MaxEnd #>= End,
    task_interval(Tasks, Starts, Ends).

%%%%%%%%%%%%%%%%%%%% Eliminate Symmetries %%%%%%%%%%%%%%%%%%%%

% eliminate_symmetries(+Tasks, +Chosen, +Start)
eliminate_symmetries(Tasks, Chosen, Start) :-
    n_machines(NumberOfMachines),
    separate_machines(NumberOfMachines, Tasks, Chosen, Start, Machines),
    eliminate_symmetries_per_machine(Machines).

% separate_machines(+MachineId, +Tasks, +Chosen, +Start, -Machines)
separate_machines(0, _, _, _, []).
separate_machines(MachineId, Tasks, Chosen, Start, [Machine | Machines]) :-
    MachineId > 0,
    get_tasks_by_machine(MachineId, Tasks, Chosen, Start, Machine),
    NextMachineId #= MachineId - 1,
    separate_machines(NextMachineId, Tasks, Chosen, Start, Machines).

% get_tasks_by_machine(+MachineId, +Tasks, +Chosen, +Start, -TasksOnTheMachine)
get_tasks_by_machine(_, [], [], [], []).
get_tasks_by_machine(MachineId, [JobId-_-_-AltTasks | Tasks], [ChosenAltTask | ChosenAltTasks], [Start | Starts], [JobId-Start | TasksOnTheMachine]) :-
    pair_element(ChosenAltTask, AltTasks, Machine-_),
    Machine #= MachineId,
    get_tasks_by_machine(MachineId, Tasks, ChosenAltTasks, Starts, TasksOnTheMachine).
get_tasks_by_machine(MachineId, [_-AltTasks | Tasks], [ChosenAltTask | ChosenAltTasks], [_ | Starts], TasksOnTheMachine) :-
    pair_element(ChosenAltTask, AltTasks, Machine-_),
    Machine #\= MachineId,
    get_tasks_by_machine(MachineId, Tasks, ChosenAltTasks, Starts, TasksOnTheMachine).

% eliminate_symmetries_per_machine(+Machines) → Machine = JobId-Start
eliminate_symmetries_per_machine([]).
eliminate_symmetries_per_machine([Machine | Machines]) :-
    order(Machine),
    eliminate_symmetries_per_machine(Machines).

% order(Machine)
order([]).
order([_]).
order([JobId1-Start1, JobId2-Start2 | TasksOnTheMachine]) :-
    JobId1 #< JobId2 #<=> Start1 #< Start2,
    order([JobId2-Start2 | TasksOnTheMachine]).
