
% reset_timer -> Clean / Reset Statistics Timer
reset_timer:-
    statistics(total_runtime, _).

% print_time(+Msg) -> Print Statistics Timer with a Message
print_time(Msg):-
    statistics(total_runtime, [_, T]),
    TS is ((T // 10) * 10) / 1000,

    write(Msg), write(TS), write('s'), nl.
