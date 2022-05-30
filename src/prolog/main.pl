
:- use_module(library(clpfd)).

:- ensure_loaded('data.pl').
:- ensure_loaded('output.pl').
:- ensure_loaded('constraints.pl').

% job shop problem
j :- jobshop.
jobshop :- jobshop(10000).
jobshop(Timeout) :-
    reset_timer,

    % Jobs and Horizon (Data)
    n_machines(Machines), get_jobs(Tasks),
    length(Tasks , N), horizon(Horizon),

    % Decision Variables
    length(Chosen, N), domain(Chosen, 0, Machines),
    length(Start , N), domain(Start , 0,  Horizon),
    length(End   , N), domain(End   , 0,  Horizon),
    Overtime in 0..Horizon,

    % Constraints
    only_one_chosen_alternative_task(   Tasks, Chosen),
    task_duration(                      Tasks, Chosen, Start, End, Duration),
    only_one_task_per_machine_at_a_time(Tasks, Chosen, Start, End),
    task_interval(                      Tasks,         Start, End),
    % eliminate_symmetries(               Tasks, Chosen, Start),

    % Solve
    % get_latest_finish( End, ObjFunc),
    get_overtime_used(Start, End, Duration, Overtime),
    append(Start  , End   , VarsAux),
    append(Chosen, VarsAux, Vars   ),
    labeling([minimize(Overtime), time_out(Timeout, Flag), assumptions(Branches)], Vars),

    % Print
    print(Tasks, Start, End, Chosen, Horizon, Overtime),
    print_time('Solving Time: '),
    fd_statistics(backtracks, Conflicts),
    write('Status ')   , write(Flag)     , nl,
    write('Conflicts '), write(Conflicts), nl,
    write('Branches ') , write(Branches) , nl.
