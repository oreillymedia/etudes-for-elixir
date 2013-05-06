%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Read in a database of phone calls and customers.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(phone_mnesia).
-export([setup/2, summary/3]).
-include("phone_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @doc Set up Mnesia tables for phone calls and customers
%% given their file names

-spec(setup(string(), string()) -> atom()).

setup(CallFileName, CustomerFileName) ->

  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:delete_table(phone_call),
  mnesia:delete_table(customer),
  
  fill_table(phone_call, CallFileName, fun add_call/1,
    record_info(fields, phone_call), bag),
  fill_table(customer, CustomerFileName, fun add_customer/1,
    record_info(fields, customer), set).

%% @doc Fill the given table with data from given file name.
%% AdderFunction assigns data to fields and writes it to the table;
%% RecordInfo is used when creating the table, as is the TableType.

fill_table(TableName, FileName, AdderFunction, RecordInfo, TableType) ->
  mnesia:create_table(TableName, [{attributes, RecordInfo}, {type, TableType}]),
  
  {OpenResult, InputFile} = file:open(FileName, [read]),
  case OpenResult of
    ok ->
      mnesia:transaction(
        fun() -> read_file(InputFile, AdderFunction) end);
    _ -> io:format("Error opening file: ~p~n", [FileName])
  end.
   
%% @doc Read a line from InputFile, and insert its contents into
%% the appropriate table by using AdderFunction.

-spec(read_file(file:io_device(), function()) -> atom()).

read_file(InputFile, AdderFunction) ->
  RawData = io:get_line(InputFile, ""),
  if
    is_list(RawData) ->
      Data = string:strip(RawData, right, $\n),
      ItemList = re:split(Data, ",", [{return, list}]),
      AdderFunction(ItemList),
      read_file(InputFile, AdderFunction);
    RawData == eof -> ok
  end.

  
%% Add a phone call record; the data is in an ItemList.

-spec(add_call(list()) -> undefined).

add_call(ItemList) ->
  [Number, SDate, STime, EDate, ETime] = ItemList,
  mnesia:write(#phone_call{phone_number = Number,
        start_date = to_date(SDate), start_time = to_time(STime),
        end_date = to_date(EDate), end_time= to_time(ETime)}).

%% Add a customer record; the data is in an ItemList.

-spec(add_customer(list()) -> undefined).

add_customer(ItemList) ->
  [Phone, Last, First, Middle, Rate] = ItemList,
  mnesia:write(#customer{phone_number = Phone, last_name = Last,
    first_name = First, middle_name = Middle, rate = to_float(Rate)}).

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

  
%% @doc Convenience routine to convert a string to float.
%% In case of an error, return zero.

-spec(to_float(string()) -> float()).

to_float(Str) ->
  {FPart, _} = string:to_float(Str),
  case FPart of
    error -> 0;
    _ -> FPart
  end.
  
summary(Last, First, Middle) ->

  QHandle = qlc:q([Customer ||
    Customer <- mnesia:table(customer),
    Customer#customer.last_name == Last,
    Customer#customer.first_name == First,
    Customer#customer.middle_name == Middle ]),
  
  {_Result, [ThePerson|_]} =
    mnesia:transaction(fun() -> qlc:e(QHandle) end),

  {_Result, Calls} = mnesia:transaction(
    fun() ->
       qlc:e(
        qlc:q( [Call ||
          Call <- mnesia:table(phone_call),
          QCustomer <- QHandle,
          QCustomer#customer.phone_number == Call#phone_call.phone_number
        ]
        )
      )
    end
  ),
  
  TotalMinutes = lists:foldl(fun subtotal/2, 0, Calls),
  
  [{ThePerson#customer.phone_number,
    TotalMinutes, TotalMinutes * ThePerson#customer.rate}].

subtotal(Item, Accumulator) ->
  StartSeconds = calendar:datetime_to_gregorian_seconds(
    {Item#phone_call.start_date, Item#phone_call.start_time}),
  EndSeconds = calendar:datetime_to_gregorian_seconds(
    {Item#phone_call.end_date, Item#phone_call.end_time}),
  Accumulator + ((EndSeconds - StartSeconds + 59) div 60).

