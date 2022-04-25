
:- use_module(library(clpfd)).

:- ensure_loaded('helpers.pl').
:- ensure_loaded('data.pl').
:- ensure_loaded('output.pl').
:- ensure_loaded('constraints.pl').

% job shop problem
jobshop :-
    reset_timer,

    % Jobs and Horizon (Data)
    get_n_machines(Machines),
    get_jobs(Jobs)   , get_tasks(  Jobs ,   Tasks),
    length(Tasks , N), get_horizon(Tasks, Horizon),

    % Decision Variables
    length(Chosen, N), domain(Chosen, 0, Machines),
    length(Start , N), domain(Start , 0,  Horizon),
    length(End   , N), domain(End   , 0,  Horizon),

    % Constraints
    only_one_chosen_alternative_task(   Tasks, Chosen),
    write(Chosen), nl,
    task_duration(                      Tasks, Chosen, Start, End), % TODO: verify that the Chosen is not initialized here
    write(Chosen), nl,
    only_one_task_per_machine_at_a_time(Tasks, Chosen, Start, End), % TODO: it is stopping the execution
    write(Chosen), nl,

    % Solve
    get_latest_finish( End, ObjFunc),
    append(Start  , End   , VarsAux),
    append(Chosen, VarsAux, Vars   ),
    labeling([], Vars),

    % Print
    print(Tasks, Start, End, Chosen, Horizon, ObjFunc).
