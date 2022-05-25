
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
    maximum(Partial, AltTaskDurations),
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

%%%%%%%%%%%%%%%%%%%% OVERTIME %%%%%%%%%%%%%%%%%%%%

% get_normal_time(-NormalTime)
get_normal_time(1920). % TODO: maybe not hardcoded?

% get_over_time(+Overtime)
get_over_time(384). % TODO: maybe not hardcoded?

% get_week_time(+WeekTime)
get_week_time(WeekTime) :-
    get_normal_time(NormalTime),
    get_over_time(Overtime),
    WeekTime is NormalTime + Overtime.

% get_overtime_used(+Start, +End, +Duration, -Overtime)
get_overtime_used(Start, End, Duration, Overtime) :-
    get_overtime_used(Start, End, Duration, 0, Overtime).

% get_overtime_used(+Start, +End, +Duration, +Temp, -Overtime)
get_overtime_used([], [], [], Overtime, Overtime).
get_overtime_used([Start | Starts], [End | Ends], [Duration | Durations], Temp, Overtime) :-
    get_task_used_overtime(Start, End, Duration, TaskOvertimeUsed),
    NextTemp #= Temp + TaskOvertimeUsed,
    get_overtime_used(Starts, Ends, Durations, NextTemp, Overtime).

% get_task_used_overtime(+Start, +End, +Duration, -TaskOvertimeUsed)
get_task_used_overtime(Start, End, Duration, TaskOvertimeUsed) :-
    UnusedOvertime #= End - Start - Duration,
    get_total_overtime(Start, End, TotalOvertime),
    TaskOvertimeUsed #= TotalOvertime - UnusedOvertime.

get_total_overtime(Start, End, TotalOvertime) :-
    get_week_time(WeekTime),
    get_over_time(OverTime),
    get_normal_time(NormalTime),
    
    StartMod   #= Start mod WeekTime,
    EndMod     #=  End  mod WeekTime,
    StartAlign #= Start - StartMod,
    EndAlign   #=  End  -   EndMod + WeekTime,
    StartWeek  #= StartAlign / WeekTime,
    EndWeek    #=   EndAlign / WeekTime,

    % Middle Overtime
    Weeks      #= EndWeek - StartWeek,
    MiddleOvertime #= Weeks * OverTime,

    % Unused Start Overtime
    StartDiff #= StartMod - NormalTime,
    maximum(StartOvertime, [StartDiff, 0]),

    % Unused End Overtime
    EndDiff   #= WeekTime - EndMod,
    minimum(EndOvertime, [EndDiff, 0]),

    TotalOvertime #= MiddleOvertime - StartOvertime - EndOvertime.
