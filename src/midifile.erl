-module(midifile).
-export([read/1, write/2]).
-author("Jim Menard, jim@jimmenard.com").
-include("midi_consts.hrl").

%% This module reads and writes MIDI files.

%-define(DEBUG, true).
-ifdef(DEBUG).
-define(DPRINT(X, Y), io:format(X, Y)).
-else.
-define(DPRINT(X, Y), void).
-endif.


%% Returns
%%   {seq, {header...}, ConductorTrack, ListOfTracks}
%% header is {header, Format, Division}
%% ConductorTrack is the first track of a format 1 MIDI file
%% each track including the conductor track is
%%   {track, ListOfEvents}
%% each event is
%%   {event_name, DeltaTime, [values...]}
%% where values after DeltaTime are specific to each event type. If the value
%% is a string, then the string appears instead of [values...].
read(Path) ->
    case file:read_file(Path) of
	{ok, Bin} ->
        binary_to_seq(Bin);
    {error, Reason} ->
	    {error, Reason}
    end.

binary_to_seq(Bin) ->
    % Look for Cookie
    % Bin2 is after Cookie
    [_Skip, Bin2] = binary:split(Bin, <<"MThd">>),
    {Header, NumTracks, Bin3} = parse_header(Bin2),
    {Tracks, _Bin4} = read_tracks(NumTracks, [], Bin3),
    [ConductorTrack | RemainingTracks] = Tracks,
    {seq, Header, ConductorTrack, RemainingTracks}.

parse_header(<<_BytesToRead:32/integer, Format:16/integer,
		   NumTracks:16/integer, Division:16/integer, Bin/binary>>) ->
   {{header, Format, Division}, NumTracks, Bin}.

read_tracks(0, Tracks, Bin) ->
    {lists:reverse(Tracks), Bin};
read_tracks(NumTracks, Tracks, Bin) ->
    {Track, Bin2} = read_track(Bin),
    read_tracks(NumTracks - 1, [Track|Tracks], Bin2).

read_track(Bin) ->
    [_Skip, Bin2] = binary:split(Bin, <<"MTrk">>),
    {BytesToRead, Bin3} = parse_track_header(Bin2),
    {TrackBin, Bin4} = erlang:split_binary(Bin3, BytesToRead),
    Track = {track, event_list(TrackBin)},
    {Track, Bin4}.

parse_track_header(<<BytesToRead:32/integer, Bin/binary>>) ->
    {BytesToRead, Bin}.

event_list(Bin) ->
    event_list(0, 0, [], Bin).

event_list(_Status, _Chan, Events, <<>>) ->
    lists:reverse(Events);
event_list(Status, Chan, Events, Bin) ->
    {_, DeltaTime, Bin2} = read_var_len(Bin),
    {Event, Status2, Chan2, Bin3} = read_event(DeltaTime, Status, Chan, Bin2),
    event_list(Status2, Chan2, [Event|Events], Bin3).

read_event(DeltaTime, _, _,
           <<?STATUS_NIBBLE_OFF:4, Chan:4, Note:8, Vel:8, Bin/binary>>) ->
    Status = ?STATUS_NIBBLE_OFF,
    Event = {off, DeltaTime, [Chan, Note, Vel]}, 
    {Event, Status, Chan, Bin};
% note on, velocity 0 is a note off
read_event(DeltaTime, _, _,
           <<?STATUS_NIBBLE_ON:4, Chan:4, Note:8, 0:8, Bin/binary>>) ->
    Status = ?STATUS_NIBBLE_ON,
    Event = {off, DeltaTime, [Chan, Note, 64]},
    {Event, Status, Chan, Bin};
read_event(DeltaTime, _, _,
           <<?STATUS_NIBBLE_ON:4, Chan:4, Note:8, Vel:8, Bin/binary>>) ->
    Status = ?STATUS_NIBBLE_ON,
    Event = {on, DeltaTime, [Chan, Note, Vel]},
    {Event, Status, Chan, Bin};
read_event(DeltaTime, _, _,
           <<?STATUS_NIBBLE_POLY_PRESS:4, Chan:4, Note:8, Amount:8, Bin/binary>>) ->
    Status = ?STATUS_NIBBLE_POLY_PRESS,
    Event = {poly_press, DeltaTime, [Chan, Note, Amount]},
    {Event, Status, Chan, Bin};
read_event(DeltaTime, _, _,
           <<?STATUS_NIBBLE_CONTROLLER:4, Chan:4, Controller:8, Value:8, Bin/binary>>) ->
    Status = ?STATUS_NIBBLE_CONTROLLER,
    Event = {controller, DeltaTime, [Chan, Controller, Value]},
    {Event, Status, Chan, Bin};
read_event(DeltaTime, _, _,
	       <<?STATUS_NIBBLE_PROGRAM_CHANGE:4, Chan:4, Program:8, Bin/binary>>) ->
    Status = ?STATUS_NIBBLE_PROGRAM_CHANGE,
    Event = {program, DeltaTime, [Chan, Program]},
    {Event, Status, Chan, Bin};
read_event(DeltaTime, _, _,
	       <<?STATUS_NIBBLE_CHANNEL_PRESSURE:4, Chan:4, Amount:8, Bin/binary>>) ->
    Status = ?STATUS_NIBBLE_CHANNEL_PRESSURE,
    Event = {chan_press, DeltaTime, [Chan, Amount]},
    {Event, Status, Chan, Bin};
read_event(DeltaTime, _, _,
	       <<?STATUS_NIBBLE_PITCH_BEND:4, Chan:4, 0:1, LSB:7, 0:1, MSB:7, Bin/binary>>) ->
    Status = ?STATUS_NIBBLE_PITCH_BEND,
    Event = {pitch_bend, DeltaTime, [Chan, <<0:2, MSB:7, LSB:7>>]},
    {Event, Status, Chan, Bin};
read_event(DeltaTime, _, _,
	       <<?STATUS_META_EVENT:8, ?META_TRACK_END:8, 0:8, Bin/binary>>) ->
    Status = ?STATUS_META_EVENT,
    Event = {track_end, DeltaTime, []},
    {Event, Status, 0, Bin};
read_event(DeltaTime, _, _,
           <<?STATUS_META_EVENT:8, Type:8, Bin/binary>>) ->
    Status = ?STATUS_META_EVENT,
    {_, Length, Bin2} = read_var_len(Bin),
    {Data, Bin3} = erlang:split_binary(Bin2, Length),
    Event = parse_meta_event(Type, DeltaTime, Data),
    {Event, Status, 0, Bin3};
read_event(DeltaTime, _, _,
           <<?STATUS_SYSEX:8, Bin/binary>>) ->
    Status = ?STATUS_SYSEX,
    {_, Length, Bin2} = read_var_len(Bin),
    {Data, Bin3} = erlang:split_binary(Bin2, Length),
    Event = {sysex, DeltaTime, [Data]},
    {Event, Status, 0, Bin3};
% Handle running status bytes
read_event(DeltaTime, Status, Chan,
           <<B0:8, B1:8, Bin/binary>>) when B0 < 128 ->
	read_event(DeltaTime, Status, Chan,
               <<Status:4, Chan:4, B0:8, B1:8, Bin/binary>>).

parse_meta_event(Type, DeltaTime, Data) ->
    case Type of
	?META_SEQ_NUM ->
	    {seq_num, DeltaTime, [Data]};
	?META_TEXT ->
	    {text, DeltaTime, binary_to_list(Data)};
	?META_COPYRIGHT ->
	    {copyright, DeltaTime, binary_to_list(Data)};
	?META_SEQ_NAME ->
	    {seq_name, DeltaTime, binary_to_list(Data)};
	?META_INSTRUMENT ->
	    {instrument, DeltaTime, binary_to_list(Data)};
	?META_LYRIC ->
	    {lyric, DeltaTime, binary_to_list(Data)};
	?META_MARKER ->
	    {marker, DeltaTime, binary_to_list(Data)};
	?META_CUE ->
	    {cue, DeltaTime, binary_to_list(Data)};
	?META_MIDI_CHAN_PREFIX ->
	    {midi_chan_prefix, DeltaTime, [Data]};
	?META_SET_TEMPO ->
	    % Data is microseconds per quarter note, in three bytes
	    <<B0:8, B1:8, B2:8>> = Data,
	    {tempo, DeltaTime, [(B0 bsl 16) + (B1 bsl 8) + B2]};
	?META_SMPTE ->
	    {smpte, DeltaTime, [Data]};
	?META_TIME_SIG ->
	    {time_signature, DeltaTime, [Data]};
	?META_KEY_SIG ->
	    {key_signature, DeltaTime, [Data]};
	?META_SEQUENCER_SPECIFIC ->
	    {seq_name, DeltaTime, [Data]};
	_ ->
	    {unknown_meta, DeltaTime, [Type, Data]}
    end.

read_var_len(<<0:1, B0:7, Bin/binary>>) ->
    {1, B0, Bin};
read_var_len(<<1:1, B0:7, 0:1, B1:7, Bin/binary>>) ->
    {2, (B0 bsl 7) + B1, Bin};
read_var_len(<<1:1, B0:7, 1:1, B1:7, 0:1, B2:7, Bin/binary>>) ->
    {3, (B0 bsl 14) + (B1 bsl 7) + B2, Bin};
read_var_len(<<1:1, B0:7, 1:1, B1:7, 1:1, B2:7, 0:1, B3:7, Bin/binary>>) ->
    {4, (B0 bsl 21) + (B1 bsl 14) + (B2 bsl 7) + B3, Bin};
read_var_len(<<1:1, B0:7, 1:1, B1:7, 1:1, B2:7, 1:1, B3:7, Bin/binary>>) ->
    {4, (B0 bsl 21) + (B1 bsl 14) + (B2 bsl 7) + B3, Bin}.

write({seq, Header, ConductorTrack, Tracks}, Path) ->
    L = [header_io_list(Header, length(Tracks) + 1) |
	 lists:map(fun(T) -> track_io_list(T) end, [ConductorTrack | Tracks])],
    ok = file:write_file(Path, L).

header_io_list(Header, NumTracks) ->
    {header, _, Division} = Header,
    ["MThd",
     0, 0, 0, 6,				% header chunk size
     0, 1,					% format,
     (NumTracks bsr 8) band 255,		% num tracks
      NumTracks        band 255,
     (Division bsr 8) band 255,			% division
      Division        band 255].

track_io_list(Track) ->
    {track, Events} = Track,
    put(status, 0),
    put(chan, 0),
    EventList =  lists:map(fun(E) -> event_io_list(E) end, Events),
    ChunkSize = chunk_size(EventList),
    ["MTrk",
     (ChunkSize bsr 24) band 255,
     (ChunkSize bsr 16) band 255,
     (ChunkSize bsr  8) band 255,
      ChunkSize         band 255,
     EventList].

% Return byte size of L, which is an IO list that contains lists, bytes, and
% binaries.
chunk_size(L) ->
    lists:foldl(fun(E, Acc) -> Acc + io_list_element_size(E) end, 0,
		lists:flatten(L)).
io_list_element_size(E) when is_binary(E) ->
    size(E);
io_list_element_size(_E) ->
    1.

event_io_list({off, DeltaTime, [Chan, Note, Vel]}) ->
    RunningStatus = get(status),
    RunningChan = get(chan),
    if
	RunningChan =:= Chan,
	    (RunningStatus =:= ?STATUS_NIBBLE_OFF orelse
	     (RunningStatus =:= ?STATUS_NIBBLE_ON andalso Vel =:= 64)) ->
	    Status = [],
	    OutVel = 0;
	true ->
	    Status = (?STATUS_NIBBLE_OFF bsl 4) + Chan,
	    OutVel = Vel,
	    put(status, ?STATUS_NIBBLE_OFF),
	    put(chan, Chan)
    end,
    [var_len(DeltaTime), Status, Note, OutVel];
event_io_list({on, DeltaTime, [Chan, Note, Vel]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_ON, Chan), Note, Vel];
event_io_list({poly_press, DeltaTime, [Chan, Note, Amount]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_POLY_PRESS, Chan), Note,
     Amount];
event_io_list({controller, DeltaTime, [Chan, Controller, Value]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_CONTROLLER, Chan),
     Controller, Value];
event_io_list({program, DeltaTime, [Chan, Program]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_PROGRAM_CHANGE, Chan),
     Program];
event_io_list({chan_press, DeltaTime, [Chan, Amount]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_CHANNEL_PRESSURE, Chan),
     Amount];
event_io_list({pitch_bend, DeltaTime, [Chan, <<0:2, MSB:7, LSB:7>>]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_PITCH_BEND, Chan),
     <<0:1, LSB:7, 0:1, MSB:7>>];
event_io_list({track_end, DeltaTime, []}) ->
    ?DPRINT("track_end~n", []),
    put(status, ?STATUS_META_EVENT),
    [var_len(DeltaTime), ?STATUS_META_EVENT, ?META_TRACK_END, 0];
event_io_list({seq_num, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_SEQ_NUM, Data);
event_io_list({text, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_TEXT, Data);
event_io_list({copyright, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_COPYRIGHT, Data);
event_io_list({seq_name, DeltaTime, Data}) ->
    put(status, ?STATUS_META_EVENT),
    meta_io_list(DeltaTime, ?META_TRACK_END, Data);
event_io_list({instrument, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_INSTRUMENT, Data);
event_io_list({lyric, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_LYRIC, Data);
event_io_list({marker, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_MARKER, Data);
event_io_list({cue, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_CUE, Data);
event_io_list({midi_chan_prefix, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_MIDI_CHAN_PREFIX, Data);
event_io_list({tempo, DeltaTime, [Data]}) ->
    ?DPRINT("tempo, data = ~p~n", [Data]),
    put(status, ?STATUS_META_EVENT),
    [var_len(DeltaTime), ?STATUS_META_EVENT, ?META_SET_TEMPO, var_len(3),
     (Data bsr 16) band 255,
     (Data bsr  8) band 255,
      Data         band 255];
event_io_list({smpte, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_SMPTE, Data);
event_io_list({time_signature, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_TIME_SIG, Data);
event_io_list({key_signature, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_KEY_SIG, Data);
event_io_list({sequencer_specific, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_SEQUENCER_SPECIFIC, Data);
event_io_list({unknown_meta, DeltaTime, [Type, Data]}) ->
    meta_io_list(DeltaTime, Type, Data).

meta_io_list(DeltaTime, Type, Data) when is_binary(Data) ->
    ?DPRINT("meta_io_list (bin) type = ~p, data = ~p~n", [Type, Data]),
    put(status, ?STATUS_META_EVENT),
    [var_len(DeltaTime), ?STATUS_META_EVENT, Type, var_len(size(Data)), Data];
meta_io_list(DeltaTime, Type, Data) ->
    ?DPRINT("meta_io_list type = ~p, data = ~p~n", [Type, Data]),
    put(status, ?STATUS_META_EVENT),
    [var_len(DeltaTime), ?STATUS_META_EVENT, Type, var_len(length(Data)), Data].

running_status(HighNibble, Chan) ->
    RunningStatus = get(status),
    RunningChan = get(chan),
    if
	RunningStatus =:= HighNibble, RunningChan =:= Chan ->
	    ?DPRINT("running status: stat = ~p, rchan = ~p~n",
		    [RunningStatus, RunningChan]),
	    [];
	true ->
	    put(status, HighNibble),
	    put(chan, Chan),
	    (HighNibble bsl 4) + Chan
    end.

var_len(I) when I < (1 bsl 7) ->
    <<0:1, I:7>>;
var_len(I) when I < (1 bsl 14) ->
    <<1:1, (I bsr 7):7, 0:1, I:7>>;
var_len(I) when I < (1 bsl 21) ->
    <<1:1, (I bsr 14):7, 1:1, (I bsr 7):7, 0:1, I:7>>;
var_len(I) when I < (1 bsl 28) ->
    <<1:1, (I bsr 21):7, 1:1, (I bsr 14):7, 1:1, (I bsr 7):7, 0:1, I:7>>;
var_len(I) ->
    exit("Value " ++ I ++ " is too big for a variable length number").
