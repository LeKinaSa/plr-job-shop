
:- ensure_loaded('../../data/fab.pl').
:- ensure_loaded('helpers.pl').

% get_jobs(-Jobs)
% Jobs = [job_id - tasks] ; tasks = [task] ; task = [alternative_task] ; alternative_task = machine_id - duration
get_jobs(Jobs) :-
    findall(Job-Tasks, job(Job, Tasks), Jobs).

% get_max_timespan(+Jobs, -Horizon)
get_max_timespan(Jobs, Horizon) :-
    get_max_timespan(Jobs, 0, Horizon).

%%%%%%%%%%%%%%% Get Max Timespan Helpers %%%%%%%%%%%%%%%

% get_max_timespan(+Jobs, +Temp, -Horizon)
get_max_timespan([], Horizon, Horizon).
get_max_timespan([_-Tasks | Jobs], Temp, Horizon) :-
    get_max_task_time(Tasks, TaskTime),
    NextTemp is Temp + TaskTime,
    get_max_timespan(Jobs, NextTemp, Horizon).

% get_max_task_time(+Tasks, -TaskTime)
get_max_task_time(Tasks, TaskTime) :-
    get_max_task_time(Tasks, 0, TaskTime).

% get_max_task_time(+Task, +Temp, -Total)
get_max_task_time([], Total, Total).
get_max_task_time([Task | Tasks], Temp, Total) :-
    get_max_alternative_task_time(Task, TaskTime),
    NextTemp is Temp + TaskTime,
    get_max_task_time(Tasks, NextTemp, Total).

% get_max_alternative_task_time(+AlternativeTasks, -Max)
get_max_alternative_task_time(AlternativeTasks, Max) :-
    get_max_alternative_task_time(AlternativeTasks, 0, Max).

% get_max_alternative_task_time(+AlternativeTasks, +Current, -Max)
get_max_alternative_task_time([], Max, Max).
get_max_alternative_task_time([_-Duration | AltTasks], Current, Max) :-
    max(Duration, Current, NewCurrent),
    get_max_alternative_task_time(AltTasks, NewCurrent, Max).
