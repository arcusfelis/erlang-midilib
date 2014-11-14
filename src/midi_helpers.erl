-module(midi_helpers).
-export([relative_to_absolute_time/1,
         absolute_to_relative_time/1]).

-export([seq_to_tracks/1,
         track_to_events/1]).

-export([split_first_track_into_channel_tracks/1,
         split_first_track_into_tracks/1]).


-type midi_time() :: non_neg_integer().
-type midi_on_event() :: {on, midi_time(), list(integer())}.
-type midi_off_event() :: {off, midi_time(), list(integer())}.
-type midi_group() :: {midi_on_event(), midi_off_event()}.

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

seq_to_first_tracks({seq,_Header,_ConductorTrack,Tracks}) ->
    hd(Tracks).

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




split_first_track_into_tracks({seq,Header,ConductorTrack,Tracks}) ->
    Seq = {seq,Header,{track,[]},[ConductorTrack|Tracks]},
    split_first_track_into_tracks_1(Seq).

split_first_track_into_tracks_1(Seq) ->
    Track1 = seq_to_first_tracks(Seq),
    SubTracks = split_track_into_tracks(Track1),
    replace_seq_tracks(SubTracks, Seq).

split_track_into_tracks(Track) ->
    Events = track_to_events(Track),
    Events1 = relative_to_absolute_time(Events),
    %% Put corresponding ON and OFF events together into pairs
    Groups = group_notes(Events1),
    OtherEvents = filter_other_events(Events1),
    %% ON events, that are in groups for sure (i.e. have corresponding pairs)
    OnEvents = groups_to_on_events(Groups),
    %% Pressed notes at each period of time
    MatchedGroupsList = [match_groups_by_time(Groups, Time) || Time <- times(OnEvents)],
    %% Pressed notes at each period of time splited into two hands
    SplitedGroupsList = [split_groups_by_hands(MatchedGroups) || MatchedGroups <- MatchedGroupsList],
    %% Put all notes together for each hand
    {LeftGroupsList, RightGroupsList} = lists:unzip(SplitedGroupsList),
    %% Flatten, sort by time, remove duplicates
    LeftGroups = lists:usort(lists:merge(LeftGroupsList)),
    RightGroups = lists:usort(lists:merge(RightGroupsList)),
    %% Right hand plays more keys in case of collisions
    %% Remove all collisions from the left hand
    LeftGroups1 = ordsets:subtract(LeftGroups, RightGroups),
    LeftEvents = groups_to_events(LeftGroups1),
    RightEvents = groups_to_events(RightGroups),
    %% Add other events into both hands
    LeftEvents1 = keymerge(2, LeftEvents, OtherEvents),
    RightEvents1 = keymerge(2, RightEvents, OtherEvents),
    %% Use standard deltas
    LeftEvents2 = absolute_to_relative_time(LeftEvents1),
    RightEvents2 = absolute_to_relative_time(RightEvents1),
    [events_to_track(RightEvents2), events_to_track(LeftEvents2)].

times(Events) ->
    [Time || {_, Time, _} <- Events]. 

%% @see lists:ukeymerge/3
keymerge(N, TupleList1, TupleList2) ->
    lists:keysort(N, TupleList1 ++ TupleList2).

groups_to_events(Groups) ->
    Events = [Event || {OnEvent, OffEvent} <- Groups, Event <- [OnEvent, OffEvent]],
    %% Sort by time
    lists:keysort(2, Events).

groups_to_on_events(Groups) ->
    [OnEvent || {OnEvent, _OffEvent} <- Groups].

split_groups_by_hands([_|_]=Groups) ->
    SortedGroups = sort_groups_by_note(Groups),
    CenterNote = find_center(SortedGroups),
    split_into_before_and_after_note(SortedGroups, CenterNote).

split_into_before_and_after_note(SortedGroups, Note) ->
    lists:splitwith(fun(Group) -> group_to_note(Group) < Note end, SortedGroups).


find_center([_|_]=SortedGroups) ->
    MinNote = group_to_note(hd(SortedGroups)),
    MaxNote = group_to_note(lists:last(SortedGroups)),
    find_center_1(MinNote, MaxNote).

find_center_1(MinNote, MaxNote) ->
    MinNote + ((MaxNote - MinNote) div 2).

sort_groups_by_note(Groups) ->
    LiftedGroups = lift_note_to_groups(Groups),
    LiftedGroups1 = lists:keysort(1, LiftedGroups),
    unlift_note_from_groups(LiftedGroups1).

lift_note_to_groups(Groups) ->
    [lift_note_to_group(Group) || Group <- Groups].

unlift_note_from_groups(LiftedGroups) ->
    [unlift_note_from_group(LiftedGroup) || LiftedGroup <- LiftedGroups].

%% Prefix with note
lift_note_to_group(Group) ->
    {group_to_note(Group), Group}.

%% Remove note prefix
unlift_note_from_group({_Note, Group}) ->
    Group.

group_to_note({OnEvent, _OffEvent}) ->
    event_to_note(OnEvent).

event_to_note({_Event,_Time,[_Chan,Note,_Vel]}) ->
    Note.

%% Select notes, that are on at time Time
match_groups_by_time(Groups, Time) ->
    [Group || Group <- Groups, is_time_between(Group, Time)].

-spec is_time_between(midi_group(), midi_time()) -> boolean().
is_time_between({{on,TimeOn,_}, {off,TimeOff,_}}, Time)
    when TimeOn =< Time, Time =< TimeOff ->
    true;
is_time_between(_, _) ->
    false.



%% Split Events into pairs
%% Requires absolute time
group_notes(Events) ->
    OnEvents = filter_on_events(Events),
    OffEvents = filter_off_events(Events),
    group_notes_1(OnEvents, OffEvents, []).

group_notes_1([OnEvent|OnEvents], OffEvents, Pairs) ->
    MaybeOffEvent = find_pair(OnEvent, OffEvents),
    group_notes_2(OnEvent, MaybeOffEvent, OnEvents, OffEvents, Pairs);
group_notes_1([], _OffEvents, Pairs) ->
    lists:reverse(Pairs).

group_notes_2(_OnEvent, undefined, OnEvents, OffEvents, Pairs) ->
    %% Ignore ON event
    group_notes_1(OnEvents, OffEvents, Pairs);
group_notes_2(OnEvent, OffEvent, OnEvents, OffEvents, Pairs) ->
    OffEvents1 = lists:delete(OffEvent, OffEvents),
    Pairs1 = [{OnEvent, OffEvent}|Pairs],
    group_notes_1(OnEvents, OffEvents1, Pairs1).

filter_on_events(Events) ->
    [Event || Event = {on, _Delta, _Args} <- Events].

filter_off_events(Events) ->
    [Event || Event = {off, _Delta, _Args} <- Events].

%% Select neither on nor off events
filter_other_events(Events) ->
    [Event || Event <- Events, is_other_event(Event)].

is_other_event({on, _Delta, _Args}) ->
    false;
is_other_event({off, _Delta, _Args}) ->
    false;
is_other_event(_) ->
    true.

find_pair({on, Delta, [Chan, Note, _Vel]}, Events) ->
    find_pair(Delta, Chan, Note, Events).

find_pair(DeltaOn, Chan, Note,
          [{off, DeltaOff, [Chan, Note, _Vel]}=Event|_Events])
        when DeltaOn =< DeltaOff ->
    Event;
find_pair(DeltaOn, Chan, Note, [_|Events]) ->
    find_pair(DeltaOn, Chan, Note, Events);
find_pair(_DeltaOn, _Chan, _Note, []) ->
    undefined.

