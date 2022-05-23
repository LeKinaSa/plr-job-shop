
:- use_module(library(clpfd)).

:- ensure_loaded('data.pl').
:- ensure_loaded('output.pl').
:- ensure_loaded('constraints.pl').

% job shop problem
jobshop :-
    reset_timer,

    % Jobs and Horizon (Data)
    n_machines(Machines), get_jobs(Tasks),
    length(Tasks , N), get_horizon(Tasks, Horizon),

    % Decision Variables
    length(Chosen, N), domain(Chosen, 0, Machines),
    length(Start , N), domain(Start , 0,  Horizon),
    length(End   , N), domain(End   , 0,  Horizon),

    % Constraints
    only_one_chosen_alternative_task(   Tasks, Chosen),
    task_duration(                      Tasks, Chosen, Start, End),
    only_one_task_per_machine_at_a_time(Tasks, Chosen, Start, End),
    task_interval(                      Tasks,         Start, End),
    eliminate_symmetries(               Tasks, Chosen, Start),

    % Solve
    get_latest_finish( End, ObjFunc),
    append(Start  , End   , VarsAux),
    append(Chosen, VarsAux, Vars   ),
    labeling([], Vars), % minimize(ObjFunc) → slower than it should be, may benefit from more constraints

    % Print
    print(Tasks, Start, End, Chosen, Horizon, ObjFunc),
    print_time('Solving Time: ').
