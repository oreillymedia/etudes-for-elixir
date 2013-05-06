%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Generate a random set of data for phone calls
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(generate_calls).
-export([make_call_list/1, format_date/1, format_time/1]).

make_call_list(N) ->
  Now = calendar:datetime_to_gregorian_seconds({{2013, 3, 10}, {9, 0, 0}}),
  Numbers = [
    {"213-555-0172", Now},
    {"301-555-0433", Now},
    {"415-555-7871", Now},
    {"650-555-3326", Now},
    {"729-555-8855", Now},
    {"838-555-1099", Now},
    {"946-555-9760", Now}
  ],
  CallList = make_call_list(N, Numbers, []),
  {Result, OutputFile} = file:open("call_list.csv", [write]),
  case Result of
    ok -> write_item(OutputFile, CallList);
    error -> io:format("Error: ~p~n", OutputFile)
  end.

make_call_list(0, _Numbers, Result) -> lists:reverse(Result);

make_call_list(N, Numbers, Result) ->
  Entry = random:uniform(length(Numbers)),
  {Head, Tail} = lists:split(Entry - 1, Numbers),
  {Number, LastCall} = hd(Tail),
  StartCall = LastCall + random:uniform(120) + 20,
  Duration = random:uniform(180) + 40,
  EndCall = StartCall + Duration,
  Item = [Number, format_date(StartCall), format_time(StartCall),
    format_date(EndCall), format_time(EndCall)],
  UpdatedNumbers = Head ++ [{Number, EndCall} | tl(Tail)],
  make_call_list(N - 1, UpdatedNumbers, [Item | Result]).

write_item(OutputFile, []) ->
  file:close(OutputFile);

write_item(OutputFile, [H|T]) ->
  io:format("~s ~s ~s ~s ~s~n", H),
  io:fwrite(OutputFile, "~s,~s,~s,~s,~s~n", H),
  write_item(OutputFile, T).

format_date(GSeconds) ->
  {Date, _Time} = calendar:gregorian_seconds_to_datetime(GSeconds),
  {Y, M, D} = Date,
  lists:flatten(io_lib:format("~4b-~2..0b-~2..0b", [Y, M, D])).

format_time(GSeconds) ->
  {_Date, Time} = calendar:gregorian_seconds_to_datetime(GSeconds),
  {M, H, S} = Time,
  lists:flatten(io_lib:format("~2..0b:~2..0b:~2..0b", [M, H, S])).
  
