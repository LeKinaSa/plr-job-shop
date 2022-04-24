
:-use_module(library(clpfd)).

:- ensure_loaded('data.pl').
:- ensure_loaded('output.pl').
:- ensure_loaded('helpers.pl').
%:- ensure_loaded('constraints.pl').

% job shop problem
jobshop :-
    reset_timer,

    % Jobs and Horizon (Data)
    get_n_machines(Machines),
    get_jobs(Jobs)   , get_tasks(Jobs, Tasks),
    length(Tasks , N), get_max_timespan(Jobs, Horizon),

    % Decision Variables
    length(Chosen, N), domain(Chosen, 0, Machines),
    length(Start , N), domain(Start , 0,  Horizon),
    length(End   , N), domain(End   , 0,  Horizon),

    % Constraints
    only_one_chosen_alternative_task(Tasks, Chosen),
    write(Tasks),
    task_duration(Tasks, Chosen, Start, End),
    %only_one_task_per_machine_at_a_time(Tasks, Chosen, Start, End),

    % Solve
    get_latest_finish(End, ObjFunc),
    append(Start  , End   , VarsAux),
    append(Chosen, VarsAux, Vars   ),
    %nl, write(Chosen),
    nl, write('Solving'), nl,
    labeling([], Vars),

    % Print
    print(Tasks, Start, End, Chosen, Horizon, ObjFunc).

task_duration([], [], [], []).
task_duration([_-Task | Tasks], [ChosenAltTask | Chosen], [Start | Starts], [End | Ends]) :-
    verify_task_duration(Task, ChosenAltTask, Start, End),
    task_duration(Tasks, Chosen, Starts, Ends).

verify_task_duration(Task, Chosen, Start, End) :-
    list_element(Chosen, Task, _-Duration),
    End #= Start + Duration.

only_one_task_per_machine_at_a_time(_, _, _, _).

only_one_chosen_alternative_task([], []).
only_one_chosen_alternative_task([_-Task | Tasks], [ChosenAltTask | Chosen]) :-
    length(Task, NumberOfAltTasks),
    ChosenAltTask in 1..NumberOfAltTasks,
    only_one_chosen_alternative_task(Tasks, Chosen).

