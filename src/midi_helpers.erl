-module(midi_helpers).
-export([relative_to_absolute_time/1,
         absolute_to_relative_time/1]).

-export([seq_to_tracks/1,
         track_to_events/1]).

-export([split_first_track_into_channel_tracks/1]).

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

seq_to_tracks({seq,_Header,_ConductorTrack,Tracks}) ->
    Tracks.

replace_seq_tracks(NewTracks, {seq,Header,ConductorTrack,_Tracks}) ->
    {seq,Header,ConductorTrack,NewTracks}.

track_to_events({track,Events}) ->
    Events.

events_to_track(Events) ->
    {track,Events}.

split_first_track_into_channel_tracks(Seq) ->
    [Track1|Tracks] = seq_to_tracks(Seq),
    SubTracks = split_track_into_channel_tracks(Track1),
    replace_seq_tracks(SubTracks ++ Tracks, Seq).

split_track_into_channel_tracks(Track) ->
    Events = track_to_events(Track),
    Events1 = relative_to_absolute_time(Events),
    [events_to_track(absolute_to_relative_time(SubEvents))
     || SubEvents <- split_events_into_channels(Events1)].

split_events_into_channels(Events) ->
    [filter_channel_events(Events, Chan) || Chan <- used_channels(Events)].

%% Return all events from both channel Chan and from unspecified channel
filter_channel_events(Events, Chan) ->
    [Event || Event <- Events, is_channel_event(Event, Chan)].

is_channel_event(Event, Chan) ->
    compare_channels(event_to_channel(Event), Chan).

compare_channels(Chan, Chan)        -> true;
compare_channels(undefined, _Chan)  -> true;
compare_channels(_OtherChan, _Chan) -> false.
    

used_channels(Events) ->
    lists:delete(undefined, lists:usort(lists:map(fun event_to_channel/1, Events))).

event_to_channel({Name, _Delta, [Chan|_]}) when
        Name =:= off;
        Name =:= on;
        Name =:= poly_press;
        Name =:= controller;
        Name =:= program;
        Name =:= chan_press;
        Name =:= pitch_bend
    ->
    Chan;
event_to_channel(_) ->
    undefined.
