
:-use_module(library(clpfd)).

:- ensure_loaded('data.pl').
:- ensure_loaded('output.pl').
:- ensure_loaded('constraints.pl').

% jobshop -> job shop with unlimited time span
jobshop :-
    % Jobs and Horizon (Data)
    get_jobs(Jobs),
    get_max_timespan(Jobs, Horizon),
    jobshop(Jobs, Horizon).

% jobshop(+Horizon) -> job shop with limited time span
jobshop(Horizon) :-
    % Jobs (Data)
    get_jobs(Jobs),
    jobshop(Jobs, Horizon).

% jobshop(+Jobs, +Horizon) -> job shop solver
jobshop(Jobs, Horizon) :-
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
