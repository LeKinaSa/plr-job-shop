
:- use_module(library(clpfd)).

:- ensure_loaded('data.pl').
:- ensure_loaded('output.pl').
:- ensure_loaded('constraints.pl').

% job shop problem
jobshop :-
    % Data (Jobs, Tasks, Machines and Horizon)
    n_machines(Machines),
    get_jobs(Jobs)   , get_tasks(  Jobs ,   Tasks),
    length(Tasks , N), get_horizon(Tasks, Horizon),

    % Reset Timer
    reset_timer,

    % Find a possible solution
    jobshop_optimizer(Machines-Tasks-N, Horizon, 0).

% jobshop_optimizer(+JobsData, +Horizon, +Iteration)
%   job shop optimizer ; finds the best solution to the job shop problem
jobshop_optimizer(JobsData, Horizon, Iteration) :-
    % Print Iteration Number
    nl, nl, write('         Iteration '), write(Iteration), nl,

    % Search for the Current ObjFunc
    jobshop_solver(JobsData, Horizon, ObjFunc),

    % Print Iteration Time
    print_time('         Iteration Time: '),

    % Check if there is a Solution with a Smaller ObjFunc
    NewHorizon is ObjFunc - 1,
    NewIteration is Iteration + 1,
    jobshop_optimizer(JobsData, NewHorizon, NewIteration).

jobshop_optimizer(_, _, _) :-
    % Print Iteration Time
    print_time('         Iteration Time: '),

    % Reached Optimal Solution
    write('         Found Solution'), nl, nl, nl.

% jobshop_solver(+JobsData, +Horizon, -ObjFunc) → JobsData = Machines-Tasks-TasksLength
%   job shop solver ; find a solution to the job shop problem
jobshop_solver(Machines-Tasks-N, Horizon, ObjFunc) :-
    % Decision Variables
    length(Chosen, N), domain(Chosen, 0, Machines),
    length(Start , N), domain(Start , 0,  Horizon),
    length(End   , N), domain(End   , 0,  Horizon),

    % Constraints
    only_one_chosen_alternative_task(   Tasks, Chosen),
    task_duration(                      Tasks, Chosen, Start, End),
    only_one_task_per_machine_at_a_time(Tasks, Chosen, Start, End),

    % Solve
    get_latest_finish( End, ObjFunc),
    append(Start  , End   , VarsAux),
    append(Chosen, VarsAux, Vars   ),
    labeling([], Vars),

    % Print
    print(Tasks, Start, End, Chosen, Horizon, ObjFunc).
