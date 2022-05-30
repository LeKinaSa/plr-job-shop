
:- use_module(library(file_systems)).

:- ensure_loaded('main.pl').

analyser :-
    current_directory(Directory, 'C:/Users/clara/Documents/GitHub/plr-project/src/prolog'),
    enter_files, analyser(n_jobs              , [2, 5, 10, 20, 40, 60, 80, 100, 125, 150]),
    enter_files, analyser(percent_alt_jobs    , [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]),
    enter_files, analyser(n_machines          , [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    enter_files, analyser(percent_alt_machines, [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]),
    enter_files, analyser(average_size_task   , [10, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1250, 1500, 2000, 2500]),
    enter_files, analyser(production_range    , [1, 2, 3, 4, 5, 6, 8, 10, 12, 14, 16, 18, 20]),
    enter_files, analyser(time_usage          , [50, 60, 70, 80, 90, 100]),
    enter_files, analyser(over_time_hours     , [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    enter_files, analyser(time_out            , [1, 5, 10, 15, 30, 45, 60, 120, 300]),
    current_directory(_, Directory).

analyser(_, []).
analyser(Parameter, [Value | List]) :-
    analyse(Parameter, Value),
    analyser(Parameter, List).

analyse(n_jobs, Value) :-
    analyse(Value, 50, 4, 75, 50, 2, 80, 8, 15).
analyse(percent_alt_jobs, Value) :-
    analyse(75, Value, 4, 75, 50, 2, 80, 8, 15).
analyse(n_machines, Value) :-
    analyse(75, 50, Value, 75, 50, 2, 80, 8, 15).
analyse(percent_alt_machines, Value) :-
    analyse(75, 50, 4, Value, 50, 2, 80, 8, 15).
analyse(average_size_task, Value) :-
    analyse(75, 50, 4, 75, Value, 2, 80, 8, 15).
analyse(production_range, Value) :-
    analyse(75, 50, 4, 75, 50, Value, 80, 8, 15).
analyse(time_usage, Value) :-
    analyse(75, 50, 4, 75, 50, 2, Value, 8, 15).
analyse(over_time_hours, Value) :-
    analyse(75, 50, 4, 75, 50, 2, 80, Value, 15).
analyse(time_out, Value) :-
    analyse(75, 50, 4, 75, 50, 2, 80, 8, Value).

analyse(NJobs, PercentAltJobs, NMachines, PercentAltMachines, AverageSizeTask, ProductionRange, TimeUsage, OverTimeHours, Timeout) :-
    % Clean Memory
    clean_memory,
    % Load File
    get_file_name(NJobs, PercentAltJobs, NMachines, PercentAltMachines, AverageSizeTask, ProductionRange, TimeUsage, OverTimeHours, FileName),
    ensure_loaded(FileName),
    % Jobshop
    TimeoutInMilliseconds is Timeout * 1000,
    jobshop(TimeoutInMilliseconds, Branches, Conflicts, ObjValue, Status, WallTime),
    % Write Results to File
    write_to_files([Branches, Conflicts, ObjValue, Status, WallTime]).

get_file_name(NJobs, PercentAltJobs, NMachines, PercentAltMachines,
              AverageSizeTask, ProductionRange, TimeUsage, OverTimeHours, FileName) :-
    concat(['../../data/simulated/', NJobs, '-', PercentAltJobs, '-', NMachines, '-', PercentAltMachines, '-',
            AverageSizeTask, '-', ProductionRange, '-', TimeUsage, '-', OverTimeHours, '.pl'], FileName).

clean_memory :-
    retractall(n_machines(_)),
    retractall(horizon(_)),
    retractall(normal_time(_)),
    retractall(over_time(_)),
    retractall(job(_)).

% get_files(-Files)
get_files(['../../data/statistics/prolog/branches.txt',
           '../../data/statistics/prolog/conflicts.txt',
           '../../data/statistics/prolog/obj_value.txt',
           '../../data/statistics/prolog/status.txt',
           '../../data/statistics/prolog/wall_time.txt']).

% enter_files → Place Enter at the End of the Files
enter_files :-
    get_files(Files),
    enter(Files).

% enter(+Files)
enter([]).
enter([File | Files]) :-
    write_to_file(File, [nl]),
    enter(Files).

% write_to_files(+Values) → Write Solver Statistics to Files
write_to_files(Values) :-
    get_files(Files),
    write_to_files(Files, Values).

% write_to_files(+Files, +Values)
write_to_files([], []).
write_to_files([File | Files], [Value | Values]) :-
    write_to_file(File, [', ', Value]),
    write_to_files(Files, Values).

% write_to_file(+File, +Values)
write_to_file(File, Values) :-
    open(File, write, Out),
    write_to_out(Out, Values),
    close(Out).

% write_to_out(+Out, +Values)
write_to_out(_, []).
write_to_out(Out, [Value | Values]) :-
    write(Out, Value),
    write_to_out(Out, Values).
