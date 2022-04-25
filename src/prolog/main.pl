
:- use_module(library(clpfd)).

:- ensure_loaded('data.pl').
:- ensure_loaded('output.pl').
:- ensure_loaded('constraints.pl').

% job shop problem
jobshop :-
    % Jobs and Horizon (Data)
    get_jobs(Jobs),
    get_max_timespan(Jobs, Horizon),

    % Reset Timer
    reset_timer,

    % Find a possible solution
    jobshop_optimizer(Jobs, Horizon, 0).

% jobshop_optimizer(+Jobs, +Horizon, +Iteration) → job shop optimizer ; finds the best solution to the job shop problem
jobshop_optimizer(Jobs, Horizon, Iteration) :-
    % Print Iteration Number
    nl, nl, write('         Iteration '), write(Iteration), nl,

    % Search for the Current ObjFunc
    jobshop_solver(Jobs, Horizon, ObjFunc),

    % Print Iteration Time
    print_time('         Iteration Time: '),

    % Check if there is a Solution with a Smaller ObjFunc
    NewHorizon is ObjFunc - 1,
    NewIteration is Iteration + 1,
    jobshop_optimizer(Jobs, NewHorizon, NewIteration).

jobshop_optimizer(_, _, _) :-
    % Print Iteration Time
    print_time('         Iteration Time: '),

    % Reached Optimal Solution
    write('         Found Solution'), nl, nl, nl.

% jobshop_solver(+Jobs, +Horizon, -ObjFunc) → job shop solver ; find a solution to the job shop problem
jobshop_solver(Jobs, Horizon, ObjFunc) :-
    % Tasks (Data)
    get_tasks(Jobs, Tasks),
    length(Tasks , N),

    % Decision Variables
    length(Start , N), domain(Start , 0, Horizon),
    length(End   , N), domain(End   , 0, Horizon),
    length(Chosen, N), domain(Chosen, 0,    1   ),

    % Constraints
    task_duration(Tasks, Start, End),
    only_one_chosen_alternative_task(Tasks, Chosen),
    only_one_task_per_machine_at_a_time(Tasks, Start, End, Chosen),
    
    % Solve
    append(Start  , End   , VarsAux),
    append(VarsAux, Chosen, Vars   ),
    labeling([], Vars),
    get_latest_finish(End, Chosen, ObjFunc),

    % Print
    print(Tasks, Start, End, Chosen, Horizon, ObjFunc).
