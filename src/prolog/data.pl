
:- use_module(library(clpfd)).

:- ensure_loaded('../../data/example.pl').

get_n_machines(3). % TODO: make this function

%%%%%%%%%%%%%%%%%%%%        JOBS        %%%%%%%%%%%%%%%%%%%%

% get_jobs(-Jobs)
% Jobs = [job_id - tasks] ; tasks = [task] ; task = [alternative_task] ; alternative_task = machine_id - duration
get_jobs(Jobs) :-
    findall(Job-Tasks, job(Job, Tasks), Jobs).

%%%%%%%%%%%%%%%%%%%%      HORIZON       %%%%%%%%%%%%%%%%%%%%

% get_horizon(+Tasks, -Horizon)
get_horizon(Tasks, Horizon) :-
    get_horizon(Tasks, 0, Horizon).

% get_horizon(+Tasks, +Temp, -Horizon)
get_horizon([], Horizon, Horizon).
get_horizon([_-AltTasks | Tasks], Temp, Horizon) :-
    select_second_elements(AltTasks, AltTaskDurations),
    minimum(Partial, AltTaskDurations),
    NewTemp #= Temp + Partial,
    get_horizon(Tasks, NewTemp, Horizon).

%%%%%%%%%%%%%%%%%%%%       TASKS        %%%%%%%%%%%%%%%%%%%%

% get_tasks(+Jobs, -Tasks)
get_tasks(Jobs, Tasks) :-
    get_tasks(Jobs, [], Tasks).

% get_tasks(+Jobs, +Temp, -Tasks)
get_tasks([], Tasks, Tasks).
get_tasks([Job | Jobs], Temp, Tasks) :-
    get_new_tasks(Job, NewTasks),
    append(Temp, NewTasks, NewTemp),
    get_tasks(Jobs, NewTemp, Tasks).

% get_new_tasks(+JobIdAndJobTasks, -NewTasks)
get_new_tasks(JobId-Tasks, NewTasks) :-
    get_new_tasks(JobId, 0, Tasks, NewTasks).

% get_new_tasks(+JobId, +TaskId, -JobTasks, -NewTasks)
get_new_tasks(_, _, [], []).
get_new_tasks(JobId, TaskId, [Task | Tasks], [(JobId-TaskId)-Task | NewTasks]) :-
    NextTaskId is TaskId + 1,
    get_new_tasks(JobId, NextTaskId, Tasks, NewTasks).

%%%%%%%%%%%%%%%%%%%% OBJECTIVE FUNCTION %%%%%%%%%%%%%%%%%%%%

% get_latest_finish(+Ends, -ObjFunc)
get_latest_finish(Ends, ObjFunc) :-
    maximum(ObjFunc, Ends).
