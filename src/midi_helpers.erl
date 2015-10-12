-module(midi_helpers).
-export([relative_to_absolute_time/1,
         absolute_to_relative_time/1]).

-export([seq_to_tracks/1,
         track_to_events/1]).

-export([fix_seq/1]).

-export([split_first_track_into_channel_tracks/1,
         split_first_track_into_tracks/1,
         split_first_track_into_tracks_greedy/1]).


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


%% Fix generac problems with tracks
fix_seq({seq,Header,ConductorTrack,[]}) ->
    %% Only one track
    {seq,Header,{track,[]},[ConductorTrack]};
fix_seq({seq,Header,ConductorTrack,Tracks}) ->
    {seq,Header,ConductorTrack,remove_empty_tracks(Tracks)}.

remove_empty_tracks(Tracks) ->
    [Track || Track <- Tracks, not is_empty_track(Track)].

%% Track is empty, if it does not contain at least one ON event
is_empty_track(Track) ->
    not lists:keymember(on, 1, track_to_events(Track)).


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




split_first_track_into_tracks(Seq) ->
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
    {LeftGroupsList1, RightGroupsList1} =
        minimize_difficulty_moving_groups(LeftGroupsList, RightGroupsList),
    %% Flatten, sort by time, remove duplicates
    LeftGroups = lists:usort(lists:merge(LeftGroupsList1)),
    RightGroups = lists:usort(lists:merge(RightGroupsList1)),
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

%% Returns modified groups for both left and right hands
minimize_difficulty_moving_groups(LeftGroupsList, RightGroupsList) ->
    LeftGroups = lists:usort(lists:merge(LeftGroupsList)),
    RightGroups = lists:usort(lists:merge(RightGroupsList)),
    InitDifficulty = calc_difficulty(LeftGroupsList, RightGroupsList),
    %% Move groups from left to right
    {Difficulty1, LeftGroupsList1, RightGroupsList1} =
        minimize_difficulty_moving_groups_1(LeftGroups, InitDifficulty, LeftGroupsList, RightGroupsList),
    %% Move groups from right to left
    {_Difficulty2, RightGroupsList2, LeftGroupsList2} =
        minimize_difficulty_moving_groups_1(RightGroups, Difficulty1, RightGroupsList1, LeftGroupsList1),
    {LeftGroupsList2, RightGroupsList2}.

minimize_difficulty_moving_groups_1([Group|Groups], OldDifficulty, OldFromGroupList, OldToGroupList) ->
    {NewDifficulty, NewFromGroupList, NewToGroupList} =
        minimize_difficulty_moving_groups_2(Group, OldDifficulty, OldFromGroupList, OldToGroupList),
    minimize_difficulty_moving_groups_1(Groups, NewDifficulty, NewFromGroupList, NewToGroupList);
minimize_difficulty_moving_groups_1([], Difficulty, FromGroupList, RightGroupsList) ->
    {Difficulty, FromGroupList, RightGroupsList}.

minimize_difficulty_moving_groups_2(Group, OldDifficulty, OldFromGroupList, OldToGroupList) ->
    {NewFromGroupList, NewToGroupList} = move_group(Group, OldFromGroupList, OldToGroupList),
    NewDifficulty = calc_difficulty(NewFromGroupList, NewToGroupList),
    case NewDifficulty < OldDifficulty of
        true -> % better choice
            {NewDifficulty, NewFromGroupList, NewToGroupList};
        false ->
            {OldDifficulty, OldFromGroupList, OldToGroupList}
    end.

calc_difficulty(GroupsList1, GroupsList2) ->
    D1 = calc_difficulty(GroupsList1),
    D2 = calc_difficulty(GroupsList2),
    D3 = calc_overlay_difficulty(GroupsList1, GroupsList2),
    io:format("D1 ~p~nD2 ~p~nD3 ~p~n", [D1, D2, D3]),
    D1 + D2 + D3.

calc_overlay_difficulty(GroupsList1, GroupsList2) ->
    CenterNotes1 = fix_undefined_centers(maybe_calc_centers(GroupsList1)),
    CenterNotes2 = fix_undefined_centers(maybe_calc_centers(GroupsList2)),
    Neighbors1 = zip_with_next(CenterNotes1),
    Neighbors2 = zip_with_next(CenterNotes2),
    Zipped1 = lists:zip(Neighbors1, Neighbors2),
    Zipped2 = lists:zip(Neighbors2, Neighbors1),
    lists:sum([calc_overlay_difficulty_1(X) || X <- Zipped1 ++ Zipped2]).

%% Replace undefined notes with near onces
fix_undefined_centers(CenterNotes) ->
    fix_undefined_centers_1(first_defined(CenterNotes), CenterNotes).

fix_undefined_centers_1(NearNote, [CenterNote|CenterNotes]) ->
    CenterNote2 = select_center_note(NearNote, CenterNote),
    NearNote2 = select_near_note(NearNote, CenterNote),
    [CenterNote2|fix_undefined_centers_1(NearNote2, CenterNotes)];
fix_undefined_centers_1(_NearNote, []) ->
    [].

select_center_note(NearNote, undefined) ->
    NearNote; % fix CenterNote
select_center_note(_NearNote, CenterNote) ->
    CenterNote.

select_near_note(NearNote, undefined) ->
    NearNote; % keep old NearNote
select_near_note(_NearNote, CenterNote) ->
    CenterNote. % replace NearNote with CenterNote

first_defined([undefined|T]) ->
    first_defined(T);
first_defined([H|_]) ->
    H.

calc_overlay_difficulty_1({{MyPrevCenver, MyNextCenter},
                           {OtherPrevCenver, _OtherNextCenter}})
    when is_integer(MyPrevCenver), is_integer(MyNextCenter), is_integer(OtherPrevCenver) ->
    DiffFromMyCenter = abs(MyNextCenter - MyPrevCenver),
    DiffFromOtherCenter = abs(MyNextCenter - OtherPrevCenver),
    calc_overlay_difficulty_2(DiffFromMyCenter, DiffFromOtherCenter).

calc_overlay_difficulty_2(DiffFromMyCenter, DiffFromOtherCenter)
    when DiffFromOtherCenter < DiffFromMyCenter ->
    Diff = DiffFromMyCenter - DiffFromOtherCenter,
    calc_overlay_difficulty_3(Diff); % bad, probably a wrong hand
calc_overlay_difficulty_2(_, _) ->
    0.  % ok

calc_overlay_difficulty_3(X) when X < 5 ->
    5;
calc_overlay_difficulty_3(X) when X < 10 ->
    10;
calc_overlay_difficulty_3(X) when X < 15 ->
    25;
calc_overlay_difficulty_3(_) ->
    50.


%% Move Group for each group of FromGroupList and ToGroupList
move_group(Group, FromGroupList, ToGroupList) ->
    Zipped = lists:zip(FromGroupList, ToGroupList),
    Zipped1 = [move_group_1(Group, FromGroups, ToGroups) || {FromGroups, ToGroups} <- Zipped],
    lists:unzip(Zipped1).

move_group_1(Group, FromGroups, ToGroups) ->
    case lists:member(Group, FromGroups) of
        true ->
            {lists:delete(Group, FromGroups),
             [Group|lists:delete(Group, ToGroups)]}; % add, but not duplicate
        false ->
            {FromGroups, ToGroups}
    end.

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


%% See calc_center/1
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

group_to_time({OnEvent, _OffEvent}) ->
    event_to_time(OnEvent).

event_to_note({_Event,_Time,[_Chan,Note,_Vel]}) ->
    Note.

event_to_time({_Event,Time,[_Chan,_Note,_Vel]}) ->
    Time.

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




%% Gets a list of pressed keys at each point of time for one hand
calc_difficulty(ListOfGroups) ->
    calc_difficulty_1(skip_empty_groups(ListOfGroups)).

skip_empty_groups(ListOfGroups) ->
    [Groups || [_|_]=Groups <- ListOfGroups].

calc_difficulty_1(ListOfGroups) ->
    RangeDiff = calc_range_difficulty(ListOfGroups),
    MoveDiff = calc_movement_difficulty(ListOfGroups),
    IntencityDiff = calc_intencity_difficulty(ListOfGroups),
    io:format("RD ~p~nMD ~p~nID ~p~n", [ RangeDiff, MoveDiff, IntencityDiff]),
    RangeDiff + MoveDiff + IntencityDiff.

%% Calc, how hard to spread fingers
calc_range_difficulty(ListOfGroups) ->
    lists:sum([calc_range_difficulty_1(Groups) || Groups <- ListOfGroups]).

calc_range_difficulty_1([_|_]=Groups) ->
    range_to_difficulty(calc_range(Groups)).

%% Magic numbers
range_to_difficulty(X) when X < 7 ->
    0; % trivial to play
range_to_difficulty(X) when X < 11 ->
    1; % simple to play
range_to_difficulty(X) when X < 14 ->
    5; % ok to play
range_to_difficulty(X) when X < 16 ->
    10; % playable
range_to_difficulty(X) when X < 18 ->
    20; % hard
range_to_difficulty(X) when X < 20 ->
    30; % impossible
range_to_difficulty(_) ->
    50. % wtf?

%% Calc, how many hand movements
calc_movement_difficulty(ListOfGroups) ->
    CenterNotes = calc_centers(ListOfGroups),
    Neighbors = zip_with_next(CenterNotes),
    lists:sum([centers_to_difficulty(Center1, Center2) || {Center1, Center2} <- Neighbors]).

%% Calc, how many keys to press at the same time
calc_intencity_difficulty(ListOfGroups) ->
    lists:sum([calc_intencity_difficulty_1(Groups) || Groups <- ListOfGroups]).

calc_intencity_difficulty_1(Groups) ->
    NumberOfKeys = length(Groups),
    count_to_difficulty(NumberOfKeys).

%% Magic numbers
count_to_difficulty(X) when X < 4 ->
    0; % trivial to play
count_to_difficulty(X) when X < 5 ->
    1; % ok to play
count_to_difficulty(X) when X < 6 ->
    5; % harder - all five fingers
count_to_difficulty(_) ->
    10. % One finger presses more than one key (difficult)

centers_to_difficulty(Center1, Center2) ->
    center_diff_to_difficulty(abs(Center1 - Center2)).

%% Distance between notes to move to difficulty
%% Magic numbers
center_diff_to_difficulty(X) when X < 5 ->
    0; % trivial to play
center_diff_to_difficulty(X) when X < 10 ->
    1; % ok to play
center_diff_to_difficulty(X) when X < 15 ->
    5; % less comfortable
center_diff_to_difficulty(X) when X < 20 ->
    10; % not comfortable
center_diff_to_difficulty(X) when X < 30 ->
    20; % hard
center_diff_to_difficulty(_) ->
    30.


%% How much should we spead the hand?
calc_range([_|_]=Groups) ->
    Notes = groups_to_notes(Groups),
    MinNote = lists:min(Notes),
    MaxNote = lists:max(Notes),
    MaxNote - MinNote.

%% Same as find_center/1, but unsorted groups
calc_center([_|_]=Groups) ->
    Notes = groups_to_notes(Groups),
    MinNote = lists:min(Notes),
    MaxNote = lists:max(Notes),
    find_center_1(MinNote, MaxNote).

maybe_calc_center([]) ->
    undefined;
maybe_calc_center([_|_]=Groups) ->
    Notes = groups_to_notes(Groups),
    MinNote = lists:min(Notes),
    MaxNote = lists:max(Notes),
    find_center_1(MinNote, MaxNote).

calc_centers(ListOfGroups) ->
    [calc_center(Groups) || Groups <- ListOfGroups].

maybe_calc_centers(ListOfGroups) ->
    [maybe_calc_center(Groups) || Groups <- ListOfGroups].

groups_to_notes(Groups) ->
    [group_to_note(Group) || Group <- Groups].


%% zip_with_next([1,2,3]) -> [{1,2},{2,3}].
zip_with_next([H|[Next|_]=T]) ->
    [{H,Next}|zip_with_next(T)];
zip_with_next([_]) ->
    [].



split_first_track_into_tracks_greedy(Seq) ->
    Track1 = seq_to_first_tracks(Seq),
    SubTracks = split_track_into_tracks_greedy(Track1),
    replace_seq_tracks(SubTracks, Seq).

split_track_into_tracks_greedy(Track) ->
    Events = track_to_events(Track),
    Events1 = relative_to_absolute_time(Events),
    %% Put corresponding ON and OFF events together into pairs
    Groups = group_notes(Events1),
    OtherEvents = filter_other_events(Events1),

    GroupsTuple = groups_to_tuple(Groups),
    Group2NoteIdDict = tuple_to_dict(GroupsTuple),
    %% All ON absolute times
    Times = groups_to_all_times(Groups),
    %% All keys on keyboard
    Notes = groups_to_all_notes(Groups),
    %% {Time,Note} to pressed NoteId
    TimeNote2NoteIdDict = build_time_note_to_note_id_dict(Times, Groups, Group2NoteIdDict),
    %% Matrix (Time,Note) to pressed NoteId or 0
    TimeNote2NoteIdMatrix = build_time_note_to_note_id_matrix(Times, Notes, TimeNote2NoteIdDict),
    io:format("Before:~n~160p~n", [TimeNote2NoteIdMatrix]),
    {LeftMatrix, RightMatrix} = rewrite_matrix(TimeNote2NoteIdMatrix),
    LeftGroups = matrix_to_groups(LeftMatrix, GroupsTuple),
    RightGroups = matrix_to_groups(RightMatrix, GroupsTuple),
    [groups_to_track(RightGroups, OtherEvents),
     groups_to_track(LeftGroups, OtherEvents)].

matrix_to_groups(M, GroupsTuple) ->
    NoteIds = unique_matrix_notes(M),
    [notes_id_to_group(NoteId, GroupsTuple) || NoteId <- NoteIds].

notes_id_to_group(NoteId, GroupsTuple) ->
    element(NoteId, GroupsTuple).

groups_to_track(Groups, OtherEvents) ->
    Events = groups_to_events(Groups),
    %% Add other events into both hands
    Events1 = keymerge(2, Events, OtherEvents),
    %% Use standard deltas
    Events2 = absolute_to_relative_time(Events1),
    events_to_track(Events2).

groups_to_tuple(Groups) ->
    list_to_tuple(Groups).

tuple_to_dict(Tuple) ->
    Size = tuple_size(Tuple),
    NoteIds = lists:seq(1, Size),
    Groups = tuple_to_list(Tuple),
    Pairs = lists:zip(Groups, NoteIds),
    dict:from_list(Pairs).

pairs_to_dict(Pairs) ->
    dict:from_list(Pairs).

group_to_note_id(Group, Group2NoteIdDict) ->
    dict:fetch(Group, Group2NoteIdDict).

build_time_note_to_note_id_dict(Times, Groups, Group2NoteIdDict) ->
    Pairs = build_time_note_to_note_id_pairs(Times, Groups, Group2NoteIdDict),
    pairs_to_dict(Pairs).

build_time_note_to_note_id_pairs(Times, Groups, Group2NoteIdDict) ->
    [{{Time, group_to_note(Group)},
      group_to_note_id(Group, Group2NoteIdDict)}
     || Time <- Times,
        Group <- Groups,
        is_time_between(Group, Time)].


build_time_note_to_note_id_matrix(Times, Notes, TimeNote2NoteIdDict) ->
    [[time_and_note_to_note_id(Time, Note, TimeNote2NoteIdDict) || Note <- Notes] || Time <- Times].

rewrite_matrix(M1) ->
    %% Linking is much more safe operation
    M2 = link_matrix(M1, 4),
    M3 = rewrite_matrix(M2, 10, 1, 4),
    io:format("Rewritten:~n~160p~n", [M3]),
    io:format("C:~n~160p~n", [M3]),
    %% Link again
    M4 = link_matrix(M3, 4),
    M5 = rewrite_matrix(M4, 10, 4, 12),
    io:format("Rewritten again:~n~160p~n", [M5]),
    M6 = connect_matrix(M5),
    %% With bigger rewrite range
    M7 = rewrite_matrix(M6, 16, 1, 30),
    io:format("Rewritten hardcore:~n~160p~n", [M7]),
    %% Unsplited parts in the middle left
    %% i.e.:    LL          CC             RR
    %% We should split them into:
    %%          LL          LR             RR
    M8 = split_middle_matrix(M7),
    {LeftNoteId, RightNoteId} = find_left_and_right_note_id(M8),
    LeftMatrixMask = filter_matrix(LeftNoteId, M8),
    RightMatrixMask = filter_matrix(RightNoteId, M8),
    LeftMatrix = apply_mask(LeftMatrixMask, M1),
    RightMatrix = apply_mask(RightMatrixMask, M1),
    {LeftMatrix, RightMatrix}.

find_left_and_right_note_id([Row|Rows]) ->
    NoteIds = skip_zeros(Row),
    LeftNoteId = hd(NoteIds),
    RightNoteId = lists:last(NoteIds),
    case LeftNoteId =/= RightNoteId of
        true ->
            {LeftNoteId, RightNoteId};
        false ->
            find_left_and_right_note_id(Rows)
    end.


split_middle_matrix(Rows) ->
    [split_middle_row(Row) || Row <- Rows].

split_middle_row(Row) ->
    case length(row_to_unique_note_ids(Row)) of
        0 -> % wtf?
            Row;
        1 ->
            Row;
        2 ->
            Row;
        _ ->
            split_middle_row_1(Row)
    end.

split_middle_row_1(Row) ->
    NoteIds = skip_zeros(Row),
    LeftNoteId = hd(NoteIds),
    RightNoteId = lists:last(NoteIds),
    LeftNote = min_pos(LeftNoteId, Row),
    RightNote = max_pos(RightNoteId, Row),
    assert_positive(LeftNoteId),
    assert_positive(RightNoteId),
    io:format("NoteIds ~p~n", [NoteIds]),
    Zipped = zip_row_with_note(Row),
    [rewrite_middle_note_id(Note, NoteId, LeftNote, LeftNoteId, RightNote, RightNoteId )
     || {Note, NoteId} <- Zipped].

%% Choose between left and right hand based on distance
rewrite_middle_note_id(_Note, NoteId, _LeftNote, LeftNoteId, _RightNote, RightNoteId)
    when NoteId =:= 0;
         NoteId =:= LeftNoteId;
         NoteId =:= RightNoteId ->
    NoteId;
rewrite_middle_note_id(Note, _NoteId, LeftNote, LeftNoteId, RightNote, RightNoteId) ->
    DistanceToLeft = Note - LeftNote,
    DistanceToRight = RightNote - Note,
    case DistanceToLeft < DistanceToRight of
        true ->
            LeftNoteId;
        false ->
            RightNoteId
    end.

link_matrix(M, 0) ->
    M;
link_matrix(M, Times) ->
    M2 = link_matrix(M),
    link_matrix(M2, Times-1).

link_matrix(M1) ->
    Rules = link_rules(M1),
    io:format("LinkingRules ~p~n", [Rules]),
    M2 = apply_rewrite_rules(Rules, M1),
    io:format("Linked Matrix~n~160p~n", [M2]),
    M2.

connect_matrix(M1) ->
    Rules = connect_rules(M1),
    io:format("ConnectRules ~p~n", [Rules]),
    M2 = apply_rewrite_rules(Rules, M1),
    io:format("Connected Matrix~n~160p~n", [M2]),
    M2.

link_rules(M) ->
    %% Find rows with more than 2 noteids
    Rows1 = [Row || Row <- M, length(row_to_unique_note_ids(Row)) > 2],
    %% One of middle note can be play only by one hand
    RulesPerRow = [middle_can_be_play_by_one_hand_rules(Row)
                   || Row <- Rows1],
    UsortedRules = lists:usort(lists:merge(RulesPerRow)),
    DedupRules = rewrite_duplicates(UsortedRules),
    io:format("DedupRules ~p~n", [DedupRules]),
    lists:usort(rewrite_rules(DedupRules)).

%% Connect notes for each hands
%% This function connects all distinct left notes together
%% This function connects all distinct right notes together
connect_rules(M) ->
    %% Find rows with at least 2 noteids
    Rows1 = [Row || Row <- M, length(row_to_unique_note_ids(Row)) > 1],
    %% Can be played by two hands only
    LeftRight = lists:merge([can_be_play_by_two_hand_note_ids(Row)
                             || Row <- Rows1]),
    Rules = make_connect_rules(LeftRight),
    UsortedRules = lists:usort(Rules),
    DedupRules = rewrite_duplicates(UsortedRules),
    io:format("DedupRules ~p~n", [DedupRules]),
    lists:usort(rewrite_rules(DedupRules)).

make_connect_rules(LeftRight) ->
    {LeftNoteIds, RightNoteIds} = lists:unzip(LeftRight),
    LeftRules = same_note_rules(LeftNoteIds),
    RightRules = same_note_rules(RightNoteIds),
    LeftRules ++ RightRules.

%% Replace all other notes with a highest one
%% Return usorted rules
same_note_rules([]) ->
    [];
same_note_rules(NoteIds) ->
    [MaxNoteId|OtherNotesIds] = lists:reverse(lists:usort(NoteIds)),
    Rules = [{OtherNoteId, MaxNoteId} || OtherNoteId <- OtherNotesIds],
    lists:reverse(Rules).

%% Return a list of {LeftNoteId, RightNoteId}, if it is not possible to play
%% them together with one hand only
can_be_play_by_two_hand_note_ids(Row) ->
    NoteIds = skip_zeros(Row),
    LeftNoteId = hd(NoteIds),
    RightNoteId = lists:last(NoteIds),
    LeftNote = min_pos(LeftNoteId, Row),
    RightNote = max_pos(RightNoteId, Row),
    assert_positive(LeftNoteId),
    assert_positive(RightNoteId),
    CanPlay = can_play_together(LeftNote, RightNote),
    [{LeftNoteId, RightNoteId} || not CanPlay].

middle_can_be_play_by_one_hand_rules(Row) ->
    NoteIds = skip_zeros(Row),
    LeftNoteId = hd(NoteIds),
    RightNoteId = lists:last(NoteIds),
    assert_positive(LeftNoteId),
    assert_positive(RightNoteId),
    io:format("NoteIds ~p~n", [NoteIds]),
    MiddleNoteIds = [NoteId || NoteId <- NoteIds,
                               NoteId =/= LeftNoteId,
                               NoteId =/= RightNoteId],
    LeftMidNoteId = hd(MiddleNoteIds),
    RightMidNoteId = lists:last(MiddleNoteIds),
    assert_positive(LeftMidNoteId),
    assert_positive(RightMidNoteId),
    LeftNote = min_pos(LeftNoteId, Row),
    LeftMidNote = max_pos(LeftMidNoteId, Row),
    RightNote = max_pos(RightNoteId, Row),
    RightMidNote = min_pos(RightMidNoteId, Row),
    assert_positive(LeftNote),
    assert_positive(LeftMidNote),
    assert_positive(RightNote),
    assert_positive(RightMidNote),
    CanPlayLeftMidWithLeftHand = can_play_together(LeftMidNote, LeftNote),
    CanPlayLeftMidWithRightHand = can_play_together(LeftMidNote, RightNote),
    CanPlayRightMidWithLeftHand = can_play_together(RightMidNote, LeftNote),
    CanPlayRightMidWithRightHand = can_play_together(RightMidNote, RightNote),
    CanPlayByLeftOnly = CanPlayLeftMidWithLeftHand
                and not CanPlayLeftMidWithRightHand,
    CanPlayByRightOnly = CanPlayRightMidWithRightHand
                 and not CanPlayRightMidWithLeftHand,
    [rewrite_rule(LeftNoteId, LeftMidNoteId) || CanPlayByLeftOnly]
    ++
    [rewrite_rule(RightNoteId, RightMidNoteId) || CanPlayByRightOnly].

assert_positive(X) when X > 0 ->
    ok.

can_play_together(Note1, Note2) ->
    abs(Note1 - Note2) < 15.

rewrite_rule(NoteId1, NoteId2) ->
    {min(NoteId1, NoteId2), max(NoteId1, NoteId2)}.

skip_zeros(List) ->
    [X || X <- List, X =/= 0].

replace_with_zero(X, [X|T]) ->
    [0|replace_with_zero(X, T)];
replace_with_zero(X, [H|T]) ->
    [H|replace_with_zero(X, T)];
replace_with_zero(_, []) ->
    [].

row_to_unique_note_ids(Row) ->
    lists:usort(Row) -- [0].

rewrite_matrix(M1, _LimitRange, Times, MaxTimes) when Times =:= (MaxTimes + 1) ->
    M1; % MaxTimes reached
rewrite_matrix(M1, LimitRange, Times, MaxTimes) ->
    M2 = rewrite_matrix_up(M1, LimitRange, Times),
    M3 = rewrite_matrix_left(M2, LimitRange, Times),
    M4 = rewrite_matrix_left_up(M3, LimitRange, Times),
    case count_unique_matrix_notes(M4) of
        1 -> % One hand?!
            M4;
        2 -> % Two hands! :)
            M4;
        N ->
            io:format("N=~p~n", [N]),
            % Continue again
            rewrite_matrix(M4, LimitRange, Times+1, MaxTimes)
    end.

rewrite_matrix_left(M, LimitRange, Times) ->
    Rules = note_ids_to_merge_left(M, Times),
    M2 = apply_rewrite_rules(Rules, M),
    revert_overdo_rewrites(Rules, M, M2, LimitRange, 3).

rewrite_matrix_up(M, LimitRange, Times) ->
    Rules = note_ids_to_merge_up(M, Times),
    M2 = apply_rewrite_rules(Rules, M),
    revert_overdo_rewrites(Rules, M, M2, LimitRange, 3).

rewrite_matrix_left_up(M, LimitRange, Times) ->
    Rules = note_ids_to_merge_left_up(M, Times),
    M2 = apply_rewrite_rules(Rules, M),
    revert_overdo_rewrites(Rules, M, M2, LimitRange, 3).

%% Cancel rewrites, that are impossible to play
revert_overdo_rewrites(_Rules, _M, M2, _LimitRange, 0) ->
    M2;
revert_overdo_rewrites(Rules, M, M2, LimitRange, Times) ->
    BadRules = overdo_rules(Rules, M, M2, LimitRange),
    RedoRules = Rules -- BadRules,
    M3 = apply_rewrite_rules(RedoRules, M),
    revert_overdo_rewrites(RedoRules, M, M3, LimitRange, Times-1).

overdo_rules(Rules, M, M2, LimitRange) ->
    Zipped = merge_matrices_with_zip(M, M2),
    BadRulesPerRow = [revert_overdo_rewrites_row(Rules, Row, LimitRange) || Row <- Zipped],
    lists:merge(BadRulesPerRow).

revert_overdo_rewrites_row(Rules, ZippedRow, LimitRange) ->
    NewRow = elements(2, ZippedRow),
    NewNoteIds = unique_new_rewritten_note_ids(ZippedRow),
    BadNewNoteIds = [NewNoteId || NewNoteId <- NewNoteIds,
                                  is_range_too_big(NewNoteId, NewRow, LimitRange)
                                  orelse
                                  has_splits(NewNoteId, NewRow)],
    case BadNewNoteIds of
        [] ->
            [];
        _ ->
            BadRules = [Rule||Rule={_,NewNoteId} <- Rules, lists:member(NewNoteId, BadNewNoteIds)],
            io:format("BadNewNoteIds ~p~n", [BadNewNoteIds]),
            io:format("BadRules ~p~n", [BadRules]),
            BadRules
    end.

has_splits(NewNoteId, NewRow) ->
    MinNote = min_pos(NewNoteId, NewRow),
    MaxNote = max_pos(NewNoteId, NewRow),
    Between = lists:sublist(NewRow, MinNote, MaxNote-MinNote),
    lists:any(fun(NoteId) -> NoteId =/= 0 andalso NoteId =/= NewNoteId end,
              Between).

is_range_too_big(NewNoteId, NewRow, LimitRange) ->
    MinNote = min_pos(NewNoteId, NewRow),
    MaxNote = max_pos(NewNoteId, NewRow),
    (MaxNote - MinNote) > LimitRange.

elements(N, Xs) ->
    [element(N, X) || X <- Xs].

min_pos(X, List) ->
    min_pos(X, List, 1).

min_pos(X, [X|_], N) ->
    N;
min_pos(X, [_|T], N) ->
    min_pos(X, T, N+1);
min_pos(_, [], _) ->
    0.

max_pos(X, List) ->
    max_pos(X, List, 1, 0).

max_pos(X, [X|T], N, _M) ->
    max_pos(X, T, N+1, N); % new max
max_pos(X, [_|T], N, M) ->
    max_pos(X, T, N+1, M); % old max
max_pos(_, [], _, M) ->
    M.

unique_new_rewritten_note_ids(Row) ->
    lists:usort([NewNoteId || {OldNoteId, NewNoteId} <- Row, OldNoteId =/= NewNoteId, NewNoteId =/= 0]).


count_unique_matrix_notes(M) ->
    length(unique_matrix_notes(M)).

unique_matrix_notes(M) ->
    lists:usort(lists:flatten(M)) -- [0].

apply_rewrite_rules(Rules, M) ->
    RulesDict = dict:from_list(Rules),
    F = fun(NoteId) -> get_dict_value(NoteId, RulesDict, NoteId) end,
    map_matrix(F, M).

note_ids_to_merge_left(M1, Times) ->
    M2 = shift_matrix_left(M1, Times),
    generate_rewrite_rules(M1, M2).

note_ids_to_merge_up(M1, Times) ->
    M2 = shift_matrix_up(M1, Times),
    generate_rewrite_rules(M1, M2).

note_ids_to_merge_left_up(M1, Times) ->
    M2 = shift_matrix_left(M1, Times),
    M3 = shift_matrix_up(M2, Times),
    generate_rewrite_rules(M1, M3).

generate_rewrite_rules(M1, M2) ->
    M3 = merge_matrices_with_zip(M1, M2),
    UsortedRules = note_ids_to_merge_usorted(M3),
    DedupRules = rewrite_duplicates(UsortedRules),
    lists:usort(rewrite_rules(DedupRules)).

%% Handle duplicates: [{1,2},{1,4},{2,3}] => [{1,4},{2,4},{3,4}]
%% New rule {2,4} is direct, {3,4} is inderect
rewrite_duplicates([{NoteId,NewNoteId1},{NoteId,NewNoteId2}|Rules]) ->
    %% Direct rule
    NewRule1 = {NoteId,NewNoteId2},
    %% Indirect rule
    NewRule2 = {NewNoteId1,NewNoteId2},
    %% Handle indirect rule later
    Rules2 = ordsets:add_element(NewRule1, Rules),
    Rules3 = ordsets:add_element(NewRule2, Rules2),
    %% Recurse
    rewrite_duplicates(Rules3);
rewrite_duplicates([Rule|Rules]) ->
    %% Ignore the rule
    [Rule|rewrite_duplicates(Rules)];
rewrite_duplicates([]) ->
    [].

%% Apply rules to themself: [{1,2},{2,3}] => [{1,3},{2,3}]
rewrite_rules([{NoteId1,NoteId2}|UsortedRules]) ->
    NoteId3 = find_final_replacenment(NoteId2, UsortedRules),
    Rule = {NoteId1,NoteId3},
    [Rule|rewrite_rules(UsortedRules)];
rewrite_rules([]) ->
    [].

%% find_final_replacenment(1, [{1,3},{2,1}]) => 3
find_final_replacenment(NoteId, [{NoteId,NewNoteId}|RevUsortedRules]) ->
    find_final_replacenment(NewNoteId, RevUsortedRules);
find_final_replacenment(NoteId, [{HeadNoteId,_}|RevUsortedRules])
    when NoteId < HeadNoteId -> %% not found yet
    find_final_replacenment(NoteId, RevUsortedRules);
find_final_replacenment(NoteId, _) ->
    NoteId.


note_ids_to_merge_usorted(M3) ->
    lists:usort(note_ids_to_merge(M3)).

note_ids_to_merge(M3) ->
    [{min(V1, V2), max(V1, V2)}
     || Row <- M3,
        {V1, V2} <- Row,
        V1 =/= 0,
        V2 =/= 0,
        V1 =/= V2].

merge_matrices_with_zip(M1, M2) ->
    merge_matrices_with(fun(V1,V2) -> {V1, V2} end, M1, M2).

%% Two dimention zip
merge_matrices_with(F, M1, M2) ->
    F2 = fun(Row1, Row2) ->
            lists:zipwith(F, Row1, Row2)
         end,
    lists:zipwith(F2, M1, M2).


map_matrix(F, M) ->
    F2 = fun(Row) ->
            lists:map(F, Row)
         end,
    lists:map(F2, M).

shift_matrix_left(M, 0) ->
    M;
shift_matrix_left(M, Times) when Times > 0 ->
    M1 = shift_matrix_left(M),
    shift_matrix_left(M1, Times-1).

shift_matrix_up(M, 0) ->
    M;
shift_matrix_up(M, Times) when Times > 0 ->
    M1 = shift_matrix_up(M),
    shift_matrix_up(M1, Times-1).

shift_matrix_left(M) ->
    [shift_list_left(Row) || Row <- M].

shift_matrix_up(M2) ->
    tl(M2) ++ [row_set_all_zero(hd(M2))].

shift_list_left(Row) ->
    tl(Row) ++ [0].

row_set_all_zero(Row) ->
    [0 || _ <- Row].

%% Get numeric id of pressed note (group) or 0.
time_and_note_to_note_id(Time, Note, TimeNote2NoteIdDict) ->
    get_dict_value({Time, Note}, TimeNote2NoteIdDict, 0).

get_dict_value(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

groups_to_all_times(Groups) ->
    [group_to_time(Group) || Group <- Groups].

groups_to_all_notes(Groups) ->
    Notes = groups_to_notes(Groups),
    MinNote = lists:min(Notes),
    MaxNote = lists:max(Notes),
    lists:seq(MinNote, MaxNote).

zip_row_with_note(Row) ->
    Notes = lists:seq(1, length(Row)),
    lists:zip(Notes, Row).

%% Keep only KeepNoteId, set others to zero
filter_matrix(KeepNoteId, M) ->
    map_matrix(fun(NoteId) ->
                   case NoteId of KeepNoteId -> NoteId; _ -> 0 end
               end, M).

%% Use value from M, if corresponding value from LeftMatrixMask is not zero
apply_mask(LeftMatrixMask, M) ->
    merge_matrices_with(fun(0,_) -> 0; (_, NoteId) -> NoteId end, LeftMatrixMask, M).
