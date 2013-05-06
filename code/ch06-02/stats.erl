%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for calculating basic statistics on a list of numbers.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(stats).
-export([minimum/1, maximum/1, range/1]).

%% @doc Returns the minimum item in a list of numbers. Fails when given
%% an empty list, as there's nothing reasonable to return.

-spec(minimum(list(number())) -> number()).

minimum(NumberList) ->
  [Result | Rest] = NumberList,
  minimum(Rest, Result).

minimum([], Result) -> Result;

minimum([Head|Tail], Result) ->
  case Head < Result of
    true -> minimum(Tail, Head);
    false -> minimum(Tail, Result)
  end.

%% @doc Returns the maximum item in a list of numbers. Fails when given
%% an empty list, as there's nothing reasonable to return.

-spec(maximum(list(number())) -> number()).

maximum(NumberList) ->
  [Result | Rest] = NumberList,
  maximum(Rest, Result).

maximum([], Result) -> Result;

maximum([Head|Tail], Result) ->
  case Head > Result of
    true -> maximum(Tail, Head);
    false -> maximum(Tail, Result)
  end.

%% @doc Return the range (maximum and minimum) of a list of numbers
%% as a two-element list.
-spec(range([number()]) -> [number()]).

range(NumberList) -> [minimum(NumberList), maximum(NumberList)].

