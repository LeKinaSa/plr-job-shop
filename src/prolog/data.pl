
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

%%%%%%%%%%%%%%%%%%%% OBJECTIVE FUNCTION %%%%%%%%%%%%%%%%%%%%

% get_overtime_used(+Starts, +Ends, +Durations, -OvertimeUsed)
get_overtime_used(Starts, Ends, Durations, OvertimeUsed) :-
    get_overtime_used(Starts, Ends, Durations, 0, OvertimeUsed).

% get_overtime_used(+Starts, +Ends, +Durations, +TempOvertime, -OvertimeUsed)
get_overtime_used([], [], [], OvertimeUsed, OvertimeUsed).
get_overtime_used([Start | Starts], [End | Ends], [Duration | Durations], Temp, OvertimeUsed) :-
    get_overtime_used_by_task(Start, End, Duration, OvertimeUsedByTask),
    NextTemp #= Temp + OvertimeUsedByTask,
    get_overtime_used(Starts, Ends, Durations, NextTemp, OvertimeUsed).

% get_overtime_used_by_task(+Start, +End, +Duration, -OvertimeUsedByTask)
get_overtime_used_by_task(Start, End, Duration, OvertimeUsedByTask) :-
    get_overtime_overlap(Start, End, 0-0, 0, OvertimeOverlap), % Overtime Used by the Task
    TaskExtraTime #= End - Start - Duration, % Extra Time Used by the Task (Real Duration - Production Time)
    OvertimeUsedByTask #= OvertimeOverlap - TaskExtraTime.

% get_overtime_overlap(+Start, +End, +LastOvertimeInterval, +Temp, -OvertimeOverlap)
get_overtime_overlap(_, End, LastIntervalStart-_, OvertimeOverlap, OvertimeOverlap) :-
    End #=< LastIntervalStart.
get_overtime_overlap(Start, End, LastIntervalStart-LastOvertimeEnd, Temp, OvertimeOverlap) :-
    End #> LastIntervalStart,
    get_next_overtime_interval(LastIntervalStart-LastOvertimeEnd, OvertimeInterval),
    get_overlap(OvertimeInterval, Start-End, Overlap),
    NextTemp #= Temp + Overlap,
    get_overtime_overlap(Start, End, OvertimeInterval, NextTemp, OvertimeOverlap).

% get_next_overtime_interval(+LastOvertimeInterval, -OvertimeInterval)
get_next_overtime_interval(0-0, 1920-2034).
get_next_overtime_interval(LastOvertimeStart-LastOvertimeEnd, OvertimeStart-OvertimeEnd) :-
    LastOvertimeStart #\= 0,
    OvertimeStart #= LastOvertimeStart + 2034,
    OvertimeEnd   #= LastOvertimeEnd   + 2034.

% get_overlap(+OvertimeInterval, +TaskInterval, -Overlap)
get_overlap(OS-_, _-TE, 0) :-
    TE #=< OS. % Task < Overtime
get_overlap(_-OE, TS-_, 0) :-
    OE #=< TS. % Overtime < Task
get_overlap(OS-OE, TS-TE, Overlap) :-
    TS #=< OS,
    OE #=< TE,
    Overlap #= OE - OS. % Overtime ⊆ Task: TS < OS < OE < TE
get_overlap(OS-OE, TS-TE, Overlap) :- % Should NOT be used
    OS #=< TS,
    TE #<  OE, % Avoid Task = Overtime
    Overlap #= TE - TS. % Task ⊂ Overtime: OS < TS < TE < OE
get_overlap(OS-OE, TS-TE, Overlap) :- % Should NOT be used
    OS #<  TS, % Avoid Task = Overtime
    TE #=< OE,
    Overlap #= TE - TS. % Task ⊂ Overtime: OS < TS < TE < OE
get_overlap(OS-OE, TS-TE, Overlap) :-
    TS #< OS, % Avoid Task ⊂ Overtime
    OS #< TE, % Avoid no Overlap
    TE #< OE, % Avoid Overtime ⊂ Task
    Overlap #= TE - OS. % Tasks starts outside overtime, and ends inside: TS < OS < TE < OE
get_overlap(OS-OE, TS-TE, Overlap) :- % May cause symmetries
    OS #< TS, % Avoid Overtime ⊂ Task
    TS #< OE, % Avoid no Overlap
    OE #< TE, % Avoid Task ⊂ Overtime
    Overlap #= OE - TS. % Task starts during overtime, and ends outside: OS < TS < OE < TE
