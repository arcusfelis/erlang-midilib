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
    Event = {pitch_bend, DeltaTime, [Chan, MSB, LSB]},
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
	    <<Value:24>> = Data,
	    {tempo, DeltaTime, [Value]};
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

write(Seq, Path) ->
    Encoded = seq_to_iolist(Seq),
    file:write_file(Path, Encoded).

seq_to_iolist({seq, Header, ConductorTrack, Tracks}) ->
    AllTracks = [ConductorTrack | Tracks],
    [header_to_binary(Header, length(AllTracks)) |
	 lists:map(fun track_to_binary/1, AllTracks)].

header_to_binary({header, _, Division}, NumTracks) ->
    BytesToRead = 6,
    Format = 1,
    make_header(BytesToRead, Format, NumTracks, Division).

make_header(BytesToRead, Format, NumTracks, Division) ->
    <<"MThd", BytesToRead:32, Format:16, NumTracks:16, Division:16>>.

track_to_binary(Track) ->
    {track, Events} = Track,
    put(status, 0),
    put(chan, 0),
    EventList =  encode_events(Events),
    Chunk = erlang:iolist_to_binary(EventList),
    ChunkSize = byte_size(Chunk),
    <<"MTrk", ChunkSize:32, Chunk/binary>>.

encode_events(Events) ->
    encode_events(Events, 0, 0, []).

encode_events([Event|Events], RunningChan, RunningStatus, Acc) ->
    {Encoded, RunningChan1, RunningStatus1} =
        encode_event(RunningChan, RunningStatus, Event),
    encode_events(Events, RunningChan1, RunningStatus1, [Encoded|Acc]);
encode_events([], _RunningChan, _RunningStatus, Acc) ->
    lists:reverse(Acc).


encode_event(RunningChan, RunningStatus,
             {off, DeltaTime, [Chan, Note, 64]}) ->
    %% Off with velocity=64 is on
    Status = ?STATUS_NIBBLE_ON,
    D = encode_delta(DeltaTime),
    S = encode_running_status(RunningStatus, RunningChan, Status, Chan),
    Encoded = [D, S, Note, 0],
    {Encoded, Chan, Status};
encode_event(RunningChan, RunningStatus,
             {off, DeltaTime, [Chan, Note, Vel]}) ->
    Status = ?STATUS_NIBBLE_OFF,
    D = encode_delta(DeltaTime),
    S = encode_running_status(RunningStatus, RunningChan, Status, Chan),
    Encoded = [D, S, Note, Vel],
    {Encoded, Chan, Status};
encode_event(RunningChan, RunningStatus,
             {on, DeltaTime, [Chan, Note, Vel]}) ->
    Status = ?STATUS_NIBBLE_ON,
    D = encode_delta(DeltaTime),
    S = encode_running_status(RunningStatus, RunningChan, Status, Chan),
    Encoded = [D, S, Note, Vel],
    {Encoded, Chan, Status};
encode_event(RunningChan, RunningStatus,
             {poly_press, DeltaTime, [Chan, Note, Amount]}) ->
    Status = ?STATUS_NIBBLE_POLY_PRESS,
    D = encode_delta(DeltaTime),
    S = encode_running_status(RunningStatus, RunningChan, Status, Chan),
    Encoded = [D, S, Note, Amount],
    {Encoded, Chan, Status};
encode_event(RunningChan, RunningStatus,
             {controller, DeltaTime, [Chan, Controller, Value]}) ->
    Status = ?STATUS_NIBBLE_CONTROLLER,
    D = encode_delta(DeltaTime),
    S = encode_running_status(RunningStatus, RunningChan, Status, Chan),
    Encoded = [D, S, Controller, Value],
    {Encoded, Chan, Status};
encode_event(RunningChan, RunningStatus,
             {program, DeltaTime, [Chan, Program]}) ->
    Status = ?STATUS_NIBBLE_PROGRAM_CHANGE,
    D = encode_delta(DeltaTime),
    S = encode_running_status(RunningStatus, RunningChan, Status, Chan),
    Encoded = [D, S, Program],
    {Encoded, Chan, Status};
encode_event(RunningChan, RunningStatus,
             {chan_press, DeltaTime, [Chan, Amount]}) ->
    Status = ?STATUS_NIBBLE_PROGRAM_CHANGE,
    D = encode_delta(DeltaTime),
    S = encode_running_status(RunningStatus, RunningChan, Status, Chan),
    Encoded = [D, S, Amount],
    {Encoded, Chan, Status};
encode_event(RunningChan, RunningStatus,
             {pitch_bend, DeltaTime, [Chan, Amount]}) ->
    Status = ?STATUS_NIBBLE_PROGRAM_CHANGE,
    D = encode_delta(DeltaTime),
    S = encode_running_status(RunningStatus, RunningChan, Status, Chan),
    Encoded = [D, S, Amount],
    {Encoded, Chan, Status};
encode_event(RunningChan, RunningStatus,
             {pitch_bend, DeltaTime, [Chan, MSB, LSB]}) ->
    %% TODO it probably can be simplier
    Status = ?STATUS_NIBBLE_PITCH_BEND,
    D = encode_delta(DeltaTime),
    S = encode_running_status(RunningStatus, RunningChan, Status, Chan),
    Encoded = [D, S, <<0:1, LSB:7, 0:1, MSB:7>>],
    {Encoded, Chan, Status};
encode_event(_RunningChan, _RunningStatus,
             {track_end, DeltaTime, []}) ->
    Status = ?STATUS_META_EVENT,
    D = encode_delta(DeltaTime),
    Encoded = [D, Status, ?META_TRACK_END, 0],
    {Encoded, 0, Status};

encode_event(RunningChan, RunningStatus,
             {seq_num, DeltaTime, [Data]}) ->
    Type = ?META_SEQ_NUM,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {text, DeltaTime, Data}) ->
    Type = ?META_TEXT,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {copyright, DeltaTime, Data}) ->
    Type = ?META_COPYRIGHT,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {seq_name, DeltaTime, Data}) ->
    Type = ?META_TRACK_END,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {instrument, DeltaTime, Data}) ->
    Type = ?META_INSTRUMENT,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {lyric, DeltaTime, Data}) ->
    Type = ?META_LYRIC,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {marker, DeltaTime, Data}) ->
    Type = ?META_MARKER,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {cue, DeltaTime, Data}) ->
    Type = ?META_CUE,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {midi_chan_prefix, DeltaTime, [Data]}) ->
    Type = ?META_MIDI_CHAN_PREFIX,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {tempo, DeltaTime, [Value]}) ->
    Data = <<Value:24>>,
    Type = ?META_SET_TEMPO,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {smpte, DeltaTime, [Data]}) ->
    Type = ?META_SMPTE,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {time_signature, DeltaTime, [Data]}) ->
    Type = ?META_TIME_SIG,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {key_signature, DeltaTime, [Data]}) ->
    Type = ?META_KEY_SIG,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {sequencer_specific, DeltaTime, [Data]}) ->
    Type = ?META_SEQUENCER_SPECIFIC,
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data);
encode_event(RunningChan, RunningStatus,
             {unknown_meta, DeltaTime, [Type, Data]}) ->
    encode_meta_event(RunningStatus, RunningChan, DeltaTime, Type, Data).

encode_meta_event(_RunningStatus, _RunningChan, DeltaTime, Type, Data) ->
    Status = ?STATUS_META_EVENT,
    Chan = 0,
    D = encode_delta(DeltaTime),
    L = var_len(iolist_size(Data)),
    Encoded = [D, Status, Type, L, Data],
    {Encoded, Chan, Status}.

encode_running_status(Status, Chan, Status, Chan) ->
    [];
encode_running_status(_RunningStatus, _RunningChan, Status, Chan) ->
    encode_status(Status, Chan).

% <<Status:4, Chan:4>>
encode_status(Status, Chan) ->
    (Status bsl 4) + Chan.

encode_delta(Delta) ->
    var_len(Delta).

var_len(I) when I < (1 bsl 7) ->
    <<0:1, I:7>>;
var_len(I) when I < (1 bsl 14) ->
    <<1:1, (I bsr 7):7, 0:1, I:7>>;
var_len(I) when I < (1 bsl 21) ->
    <<1:1, (I bsr 14):7, 1:1, (I bsr 7):7, 0:1, I:7>>;
var_len(I) when I < (1 bsl 28) ->
    <<1:1, (I bsr 21):7, 1:1, (I bsr 14):7, 1:1, (I bsr 7):7, 0:1, I:7>>.
