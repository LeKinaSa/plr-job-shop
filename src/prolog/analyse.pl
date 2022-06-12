
:- use_module(library(file_systems)).

:- ensure_loaded('main.pl').

% TODO: Modify to fit the current development directory
get_working_directory('/home/lekinasa/Github/plr-project/src/prolog').

analyser :-
    get_working_directory(WorkingDirectory),
    current_directory(Directory, WorkingDirectory),
    enter_files, analyser(n_jobs              , [2, 5, 10, 20, 40, 60, 80, 100, 125, 150]),
    enter_files, analyser(percent_alt_jobs    , [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]),
    enter_files, analyser(n_machines          , [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    enter_files, analyser(percent_alt_machines, [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]),
    enter_files, analyser(average_size_task   , [10, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1250, 1500, 2000, 2500]),
    enter_files, analyser(production_range    , [1, 2, 3, 4, 5, 6, 8, 10, 12, 14, 16, 18, 20]),
    enter_files, analyser(time_usage          , [50, 60, 70, 80, 90, 100]),
    enter_files, analyser(over_time_hours     , [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    enter_files, analyser(time_out            , [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300]),
    current_directory(_, Directory),
    write('Done').

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
    consult(FileName),
    % Jobshop
    TimeoutInMilliseconds is Timeout * 1000,
    jobshop(TimeoutInMilliseconds, Branches, Conflicts, ObjValue, Status, WallTime),
    % Write Results to File
    write_to_files([Branches, Conflicts, ObjValue, Status, WallTime]).

get_file_name(NJobs, PercentAltJobs, NMachines, PercentAltMachines,
              AverageSizeTask, ProductionRange, TimeUsage, OverTimeHours, FileName) :-
    concat(['../../data/simulated/', NJobs, '-', PercentAltJobs, '-', NMachines, '-', PercentAltMachines, '-',
            AverageSizeTask, '-', ProductionRange, '-', TimeUsage, '-', OverTimeHours, '.pl'], FileName).

% clean_memory → remove all data from the loaded files from memory
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
    write_to_file(File, ['\n']),
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
    open(File, append, Out),
    write_to_out(Out, Values),
    close(Out).

% write_to_out(+Out, +Values)
write_to_out(_, []).
write_to_out(Out, [Value | Values]) :-
    write(Out, Value),
    write_to_out(Out, Values).
