-module(midi_helpers).
-export([relative_to_absolute_time/1,
         absolute_to_relative_time/1]).


relative_to_absolute_time(Events) ->
    relative_to_absolute_time(Events, 0).

%% Time is time offset from the start of the track
relative_to_absolute_time([{Name, Delta, Args}|Events], Time) ->
    Time1 = Time+Delta,
    Events1 = relative_to_absolute_time(Events, Time1),
    [{Name, Time1, Args}|Events1];
relative_to_absolute_time([], _Time) ->
    [].

absolute_to_relative_time(Events) ->
    absolute_to_relative_time(Events, 0).

absolute_to_relative_time([{Name, Time1, Args}|Events], Time) ->
    Delta = Time1-Time,
    Events1 = absolute_to_relative_time(Events, Time1),
    [{Name, Delta, Args}|Events1];
absolute_to_relative_time([], _Time) ->
    [].
