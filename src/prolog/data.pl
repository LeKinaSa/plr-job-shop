
:- use_module(library(lists)).

:- ensure_loaded('../../data/fab-easy.pl').

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

% get_tasks(+Jobs, -Tasks)
get_tasks([], []).
get_tasks(Jobs, Tasks) :-
    get_all_tasks(Jobs, [], Tasks).

% get_all_tasks(+Jobs, +Temp, -Tasks)
get_all_tasks([], Tasks, Tasks).
get_all_tasks([Job | Jobs], Temp, Tasks) :-
    get_all_job_tasks(Job, JobTasks),
    append(Temp, JobTasks, NewTemp),
    get_all_tasks(Jobs, NewTemp, Tasks).

% get_job_tasks(+Job, -JobTasks)
get_all_job_tasks(JobId-Tasks, JobTasks) :-
    get_job_tasks(JobId, 0, Tasks, [], JobTasks).

% get_job_tasks(+JobId, +TaskId, +Tasks, +Temp, -JobTasks)
get_job_tasks(_, _, [], JobTasks, JobTasks).
get_job_tasks(JobId, TaskId, [Task | Tasks], Temp, JobTasks) :-
    get_job_alternative_tasks(JobId, TaskId, 0, Task, AlternativeTasks),
    append(Temp, AlternativeTasks, NewTemp),
    NewTaskId is TaskId + 1,
    get_job_tasks(JobId, NewTaskId, Tasks, NewTemp, JobTasks).

% get_job_alternative_tasks(+JobId, +TaskId, +AltId, -AltTasks)
get_job_alternative_tasks(_, _, _, [], []).
get_job_alternative_tasks(JobId, TaskId, AltId, [AltTask | AltTasks], [JobId-TaskId-AltId-AltTask | JobTasks]) :-
    NewAltId is AltId + 1,
    get_job_alternative_tasks(JobId, TaskId, NewAltId, AltTasks, JobTasks).

%%%%%%%%%%%%%%%%%%%% OBJECTIVE FUNCTION %%%%%%%%%%%%%%%%%%%%
% get_latest_finish(+Ends, +Chosen, -ObjectiveFunction)
get_latest_finish(Ends, Chosen, ObjectiveFunction) :-
    get_chosen_finishes(Ends, Chosen, [], ChosenEnds),
    maximum(ObjectiveFunction, ChosenEnds).

% get_chosen_finishes(+Ends, +Chosen, +Temp, -ChosenEnds)
get_chosen_finishes([], [], ChosenEnds, ChosenEnds).
get_chosen_finishes([End | Ends], [YesOrNo | Chosen], Temp, ChosenEnds) :-
    YesOrNo #= 1,
    get_chosen_finishes(Ends, Chosen, [End | Temp], ChosenEnds).
get_chosen_finishes([_ | Ends], [YesOrNo | Chosen], Temp, ChosenEnds) :-
    YesOrNo #= 0,
    get_chosen_finishes(Ends, Chosen, Temp, ChosenEnds).
