%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Read in a database of phone calls
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(phone_ets).
-export([setup/1, summary/0, summary/1]).
-include("phone_records.hrl").

%% @doc Create an ets table of phone calls from the given file name.

-spec(setup(string()) -> atom()).

setup(FileName) ->

  %% If the table exists, delete it
  case ets:info(call_table) of
    undefined -> false;
    _ -> ets:delete(call_table)
   end,
   
  %% and create it anew
  ets:new(call_table, [named_table, bag,
    {keypos, #phone_call.phone_number}]),
  
  {ResultCode, InputFile} = file:open(FileName, [read]),
  case ResultCode of
    ok -> read_item(InputFile);
    _ -> io:format("Error opening file: ~p~n", [InputFile])
  end.

%% Read a line from the input file, and insert its contents into
%% the call_table. This function is called recursively until end of file

-spec(read_item(file:io_device()) -> atom()).

read_item(InputFile) ->
  RawData = io:get_line(InputFile, ""),
  if
    is_list(RawData) ->
      Data = string:strip(RawData, right, $\n),
      [Number, SDate, STime, EDate, ETime] =
        re:split(Data, ",", [{return, list}]),
      ets:insert(call_table, #phone_call{phone_number = Number,
        start_date = to_date(SDate), start_time = to_time(STime),
        end_date = to_date(EDate), end_time= to_time(ETime)}),
      read_item(InputFile);
    RawData == eof -> ok
  end.

%% @doc Convert a string in form "yyyy-mm-dd" to a tuple {yyyy, mm, dd}
%% suitable for use with the calendar module.

-spec(to_date(string()) -> {integer(), integer(), integer()}).

to_date(Date) ->
  [Year, Month, Day] = re:split(Date, "-", [{return, list}]),
  [{Y, _}, {M, _}, {D, _}] = lists:map(fun string:to_integer/1,
    [Year, Month, Day]),
  {Y, M, D}.

%% @doc Convert a string in form "hh:mm:ss" to a tuple {hh, mm, ss}
%% suitable for use with the calendar module.

-spec(to_time(string()) -> {integer(), integer(), integer()}).

to_time(Time) ->
  [Hour, Minute, Second] = re:split(Time, ":", [{return, list}]),
  [{H, _}, {M, _}, {S, _}] = lists:map(fun string:to_integer/1,
    [Hour, Minute, Second]),
  {H, M, S}.

%% @doc Create a summary of number of minutes used by all phone numbers.

-spec(summary() -> [tuple(string(), integer())]).

summary() ->
  FirstKey = ets:first(call_table),
  summary(FirstKey, []).

summary(Key, Result) ->
  NextKey = ets:next(call_table, Key),
  case NextKey of
    '$end_of_table' -> Result;
    _ -> summary(NextKey, [hd(summary(Key)) | Result])
  end.

%% @doc Create a summary of number of minutes used by one phone number.

-spec(summary(string()) -> [tuple(string(), integer())]).

summary(PhoneNumber) ->
  Calls = ets:lookup(call_table, PhoneNumber),
  Total = lists:foldl(fun subtotal/2, 0, Calls),
  [{PhoneNumber, Total}].

subtotal(Item, Accumulator) ->
  StartSeconds = calendar:datetime_to_gregorian_seconds(
    {Item#phone_call.start_date, Item#phone_call.start_time}),
  EndSeconds = calendar:datetime_to_gregorian_seconds(
    {Item#phone_call.end_date, Item#phone_call.end_time}),
  Accumulator + ((EndSeconds - StartSeconds + 59) div 60).
  
