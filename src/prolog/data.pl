
:- use_module(library(clpfd)).

:- ensure_loaded('../../data/fab.pl').

%%%%%%%%%%%%%%%%%%%%        JOBS        %%%%%%%%%%%%%%%%%%%%

% get_jobs(-Jobs)
% Jobs = [ (JobId - MinStart - MaxEnd) - Task] ; Task = [AlternativeTask] ; AlternativeTask = MachineId - Duration
get_jobs(Jobs) :-
    findall(Job, job(Job), Jobs).

%%%%%%%%%%%%%%%%%%%%      HORIZON       %%%%%%%%%%%%%%%%%%%%

% get_horizon(+Tasks, -Horizon)
get_horizon(Tasks, Horizon) :-
    get_horizon(Tasks, 0, Horizon).

% get_horizon(+Tasks, +Temp, -Horizon)
get_horizon([], Horizon, Horizon).
get_horizon([_-AltTasks | Tasks], Temp, Horizon) :-
    divide_list(AltTasks, _, AltTaskDurations),
    minimum(Partial, AltTaskDurations),
    NewTemp #= Temp + Partial,
    get_horizon(Tasks, NewTemp, Horizon).

%%%%%%%%%%%%%%%%%%%%       TASKS        %%%%%%%%%%%%%%%%%%%%

% get_tasks(+Jobs, -Tasks)
get_tasks([], [], [], []).
get_tasks([(_-MinStart-MaxEnd)-Task | Jobs], [MinStart | Mins], [MaxEnd | Maxs], [Task | Tasks]) :-
    get_tasks(Jobs, Mins, Maxs, Tasks).

%%%%%%%%%%%%%%%%%%%% OBJECTIVE FUNCTION %%%%%%%%%%%%%%%%%%%%

% get_latest_finish(+Ends, -ObjFunc)
get_latest_finish(Ends, ObjFunc) :-
    maximum(ObjFunc, Ends).
