%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for raising a number to an integer power.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(powers_traced).
-export([raise/2]).

%% @doc Raise a number X to an integer power N.
%% Any number to the power 0 equals 1.
%% Any number to the power 1 is that number itself.
%% When N is negative, X^N is equal to 1.0 / X^N
%% When N is positive, call raise/3 with 1 as the accumulator.

-spec(raise(number(), integer()) -> number()).

raise(_, 0) -> 1;

raise(X, N) when N > 0 ->
  raise(X, N, 1);

raise(X, N) when N < 0 -> 1 / raise(X, -N).

%% @doc Helper function to raise X to N by passing an Accumulator
%% from call to call.
%% When N is 0, return the value of the Accumulator;
%% otherwise return raise(X, N - 1, X * Accumulator)

-spec(raise(number(), integer(), number()) -> number()).

raise(_, 0, Accumulator) ->
  io:format("N equals 0."),
  Result = Accumulator,
  io:format("Result is ~p~n", [Result]),
  Result;

raise(X, N, Accumulator) ->
  io:format("Enter: X is ~p, N is ~p, Accumulator is ~p~n",
    [X, N, Accumulator]),
  Result = raise(X, N-1, X * Accumulator),
  io:format("Result is ~p~n", [Result]),
  Result.

