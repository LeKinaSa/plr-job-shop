:- ensure_loaded('../../data/fab.pl').

% get_data(-Products, -Machines, -Horizon)
get_data(Products, Machines, Horizon) :-
    findall(ProductId-Duration-Total-Lines, model(ProductId, Duration, Total, Lines), Products),
    findall(LineId-Capacity, line(LineId, Capacity), Machines),
    get_max_timespan(Products, Horizon).

% get_max_timespan(+Products, -Horizon)
get_max_timespan([], 0).
get_max_timespan([ _-Duration-_-_ | Products], Horizon) :-
    Horizon is Duration + PrevHorizon,
    get_max_timespan(Products, PrevHorizon).
