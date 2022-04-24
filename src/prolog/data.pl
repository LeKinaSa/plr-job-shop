
:- use_module(library(lists)).

:- ensure_loaded('../../data/example.pl').

get_n_machines(3). % TODO: make this function

%%%%%%%%%%%%%%%%%%%%        JOBS        %%%%%%%%%%%%%%%%%%%%

% get_jobs(-Jobs)
% Jobs = [job_id - tasks] ; tasks = [task] ; task = [alternative_task] ; alternative_task = machine_id - duration
get_jobs(Jobs) :-
    findall(Job-Tasks, job(Job, Tasks), Jobs).

%%%%%%%%%%%%%%%%%%%%      HORIZON       %%%%%%%%%%%%%%%%%%%%

% get_max_timespan(+Jobs, -Horizon)
get_max_timespan(Jobs, Horizon) :-
    get_max_timespan(Jobs, 0, Horizon).

% get_max_timespan(+Jobs, +Temp, -Horizon)
get_max_timespan([], Horizon, Horizon).
get_max_timespan([_-Tasks | Jobs], Temp, Horizon) :-
    get_task_time(Tasks, TaskTime),
    NextTemp is Temp + TaskTime,
    get_max_timespan(Jobs, NextTemp, Horizon).

% get_task_time(+Tasks, -TaskTime)
get_task_time(Tasks, TaskTime) :-
    get_task_time(Tasks, 0, TaskTime).

% get_task_time(+Task, +Temp, -Total)
get_task_time([], Total, Total).
get_task_time([Task | Tasks], Temp, Total) :-
    get_min_alternative_task_time(Task, TaskTime),
    NextTemp is Temp + TaskTime,
    get_task_time(Tasks, NextTemp, Total).

% get_min_alternative_task_time(+AlternativeTasks, -Min)
get_min_alternative_task_time([M-D | AlternativeTasks], Min) :-
    get_min_alternative_task_time([M-D | AlternativeTasks], D, Min).

% get_min_alternative_task_time(+AlternativeTasks, +Current, -Max)
get_min_alternative_task_time([], Min, Min).
get_min_alternative_task_time([_-Duration | AltTasks], Current, Min) :-
    min_member(NewCurrent, [Duration, Current]),
    get_min_alternative_task_time(AltTasks, NewCurrent, Min).

%%%%%%%%%%%%%%%%%%%%       TASKS        %%%%%%%%%%%%%%%%%%%%

get_tasks(Jobs, Tasks) :-
    get_tasks(Jobs, [], Tasks).

get_tasks([], Tasks, Tasks).
get_tasks([Job | Jobs], Temp, Tasks) :-
    get_new_tasks(Job, NewTasks),
    append(Temp, NewTasks, NewTemp),
    get_tasks(Jobs, NewTemp, Tasks).

get_new_tasks(JobId-Tasks, NewTasks) :-
    get_new_tasks(JobId, 0, Tasks, NewTasks).

get_new_tasks(_, _, [], []).
get_new_tasks(JobId, TaskId, [Task | Tasks], [(JobId-TaskId)-Task | NewTasks]) :-
    NextTaskId is TaskId + 1,
    get_new_tasks(JobId, NextTaskId, Tasks, NewTasks).

%%%%%%%%%%%%%%%%%%%% OBJECTIVE FUNCTION %%%%%%%%%%%%%%%%%%%%

% get_latest_finish(+Ends, -ObjFunc)
get_latest_finish(Ends, ObjFunc) :-
    maximum(ObjFunc, Ends).
