
% print(+Tasks, +Starts, +Ends, +Chosen, +Horizon, +ObjectiveFunctionValue)
print(Tasks, Start, End, Chosen, Horizon, ObjFunc) :-
    % divide_tasks(Tasks, Machines, Interval),
    %                                    nl,
    % write('Machines '), write(Machines), nl,
    % write('Interval '), write(Interval), nl,
    % write('Start    '), write(Start   ), nl,
    % write('End      '), write(End     ), nl,
    % write('Chosen   '), write(Chosen  ), nl,
    % write('Horizon  '), write(Horizon ), nl,
    write('ObjFunc  '), write(ObjFunc ), nl.

% divide_tasks(+Tasks, -Machines, -Intervals)
divide_tasks([], [], []).
divide_tasks([_-(M-I) | Tasks], [M | Machines], [I | Intervals]) :-
    divide_tasks(Tasks, Machines, Intervals).
