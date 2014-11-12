-module(midi_helpers_tests).
-compile([export_all]).


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type event_name() :: on | off | tempo | controller | track_end.
-type event_delta() :: non_neg_integer().
-type event() :: {event_name(), event_delta(), list(0 .. 255)}.
-type events() :: list(event()).

prop_inverse_relative_to_absolute_time() ->
    ?FORALL(Events, events(),
        begin
        equals(midi_helpers:absolute_to_relative_time(
                   midi_helpers:relative_to_absolute_time(Events)),
               Events)
        end).

prop_monotonically_increasing() ->
    ?FORALL(Events, non_empty(events()),
        ?FORALL(A, integer(1, length(Events)),
            ?FORALL(B, integer(A, length(Events)),
                begin
                Events1 = midi_helpers:relative_to_absolute_time(Events),
                {_, AbsTimeA, _} = lists:nth(A, Events1),
                {_, AbsTimeB, _} = lists:nth(B, Events1),
                AbsTimeA =< AbsTimeB
                end))).

%% -------------------------------------------------------------------
%% Property Testing
%% -------------------------------------------------------------------

run_property_testing_test() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, [{constraint_tries, 500}]),
    erlang:group_leader(EunitLeader, self()),
    ?assertEqual([], Res). 


