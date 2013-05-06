%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Read in a database of people and their pets
%% appointments.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(pet_mnesia).
-export([setup/2, get_info/0, get_info_easier/0]).
-include("pet_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @doc Set up Mnesia tables for phone calls and customers
%% given their file names

-spec(setup(string(), string()) -> atom()).

setup(PersonFileName, AnimalFileName) ->

  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:delete_table(person),
  mnesia:delete_table(animal),
  
  fill_table(person, PersonFileName, fun add_person/1,
    record_info(fields, person), set),
  fill_table(animal, AnimalFileName, fun add_animal/1,
    record_info(fields, animal), set).

%% @doc Fill the given table with data from given file name.
%% AdderFunction assigns data to fields and writes it to the table;
%% RecordInfo is used when creating the table, as is the TableType.

fill_table(TableName, FileName, AdderFunction, RecordInfo, TableType) ->
  mnesia:create_table(TableName, [{attributes, RecordInfo}, {type, TableType}]),
  
  {OpenResult, InputFile} = file:open(FileName, [read]),
  case OpenResult of
    ok ->
      TransResult = mnesia:transaction(
        fun() -> read_file(InputFile, AdderFunction) end),
        io:format("Transaction result ~p~n", [TransResult]);
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

  
%% Add a person record; the data is in an ItemList.

-spec(add_person(list()) -> undefined).

add_person(ItemList) ->
  [Id, Name, Age, Gender, City, Owed] = ItemList,
  mnesia:write(#person{id_number = to_int(Id), name = Name,
    age = to_int(Age), gender = Gender, city = City,
    amount_owed = to_float(Owed)}).

%% Add an animal record; the data is in an ItemList.

-spec(add_animal(list()) -> undefined).

add_animal(ItemList) ->
  [Id, Name, Species, Gender, Owner] = ItemList,
  mnesia:write(#animal{id_number = to_int(Id),
    name = Name, species = Species, gender = Gender,
    owner_id = to_int(Owner)}).

%% @doc Convenience routine to convert a string to integer.
%% In case of an error, return zero.

-spec(to_int(string()) -> integer()).

to_int(Str) ->
  {IPart, _} = string:to_integer(Str),
  case IPart of
    error -> 0;
    _ -> IPart
  end.

%% @doc Convenience routine to convert a string to float.
%% In case of an error, return zero.

-spec(to_float(string()) -> float()).

to_float(Str) ->
  {FPart, _} = string:to_float(Str),
  case FPart of
    error -> 0;
    _ -> FPart
  end.

get_info() ->
  People = mnesia:transaction(
    fun() -> qlc:e(
      qlc:q( [ P ||
        P <- mnesia:table(person),
        P#person.age >= 21,
        P#person.gender == "M",
        P#person.city == "Podunk"]
        )
      )
    end
  ),
  
  Pets = mnesia:transaction(
    fun() -> qlc:e(
      qlc:q( [{A#animal.name, A#animal.species, P#person.name} ||
        P <- mnesia:table(person),
        P#person.age >= 21,
        P#person.gender == "M",
        P#person.city == "Podunk",
        A <- mnesia:table(animal),
        A#animal.owner_id == P#person.id_number])
      )
    end
  ),
  [People, Pets].

get_info_easier() ->
  
  %% "Pre-process" the list comprehension for finding people
  
  QHandle = qlc:q( [ P ||
    P <- mnesia:table(person),
    P#person.age >= 21,
    P#person.gender == "M",
    P#person.city == "Podunk"]
  ),
  
  %% Evaluate it to retrieve the people you want
  
  People = mnesia:transaction(
    fun() -> qlc:e( QHandle ) end
  ),
  
  %% And use the handle again when retrieving
  %% information about their pets
  
  Pets = mnesia:transaction(
    fun() -> qlc:e(
      qlc:q( [{A#animal.name, A#animal.species, P#person.name} ||
        P <- QHandle,
        A <- mnesia:table(animal),
        A#animal.owner_id == P#person.id_number])
      )
    end
  ),
  [People, Pets].



