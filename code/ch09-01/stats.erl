%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for calculating basic statistics on a list of numbers.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(stats).
-export([minimum/1, maximum/1, range/1, mean/1, stdv/1, stdv_sums/2]).

%% @doc Returns the minimum item in a list of numbers. Uses
%% try/catch to return an error when there's an empty list,
%% as there's nothing reasonable to return.

-spec(minimum(list()) -> number()).

minimum(NumberList) ->
  try minimum(NumberList, hd(NumberList)) of
    Answer -> Answer
  catch
    error:Error -> {error, Error}
  end.

minimum([], Result) -> Result;

minimum([Head|Tail], Result) ->
  case Head < Result of
    true -> minimum(Tail, Head);
    false -> minimum(Tail, Result)
  end.

%% @doc Returns the maximum item in a list of numbers. Catches
%% errors when given an empty list.

-spec(maximum(list()) -> number()).

maximum(NumberList) ->
  try
    maximum(NumberList, hd(NumberList))
  catch
    error:Error-> {error, Error}
  end.

maximum([], Result) -> Result;

maximum([Head|Tail], Result) ->
  case Head > Result of
    true -> maximum(Tail, Head);
    false -> maximum(Tail, Result)
  end.

%% @doc Return the range (maximum and minimum) of a list of numbers
%% as a two-element list.
-spec(range(list()) -> list()).

range(NumberList) -> [minimum(NumberList), maximum(NumberList)].

%% @doc Return the mean of the list.
-spec(mean(list()) -> float()).

mean(NumberList) ->
  try
    Sum = lists:foldl(fun(V, A) -> V + A end, 0, NumberList),
    Sum / length(NumberList)
  catch
    error:Error -> {error, Error}
  end.

%% @doc Helper function to generate sums and sums of squares
%% when calculating standard deviation.

-spec(stdv_sums(number(),[number()]) -> [number()]).

stdv_sums(Value, Accumulator) ->
  [Sum, SumSquares] = Accumulator,
  [Sum + Value, SumSquares + Value * Value].

%% @doc Calculate the standard deviation of a list of numbers.

-spec(stdv([number()]) -> float()).

stdv(NumberList) ->
  N = length(NumberList),
  try
    [Sum, SumSquares] = lists:foldl(fun stdv_sums/2, [0, 0], NumberList),
    math:sqrt((N * SumSquares - Sum * Sum) / (N * (N - 1)))
  catch
    error:Error -> {error, Error}
  end.

