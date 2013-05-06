%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for raising a number to an integer power
%% and finding the Nth root of a number using Newton's method.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(powers).
-export([nth_root/2, raise/2]).

%% @doc Find the nth root of a given number.

-spec(nth_root(number(), integer()) -> number()).

nth_root(X, N) ->
  A = X / 2.0,
  nth_root(X, N, A).

%% @doc Helper function to find an nth_root by passing
%% an approximation from one call to the next.
%% If the difference between current and next approximations
%% is less than 1.0e-8, return the next approximation; otherwise return
%% nth_root(X, N, NextApproximation).

nth_root(X, N, A) ->
  io:format("Current guess is ~p~n", [A]), %% see the guesses converge
  F = raise(A, N) - X,
  Fprime = N * raise(A, N - 1),
  Next = A - F / Fprime,
  Change = abs(Next - A),
  if
    Change < 1.0e-8 -> Next;
    true -> nth_root(X, N, Next)
  end.

%% @doc Raise a number X to an integer power N.
%% Any number to the power 0 equals 1.
%% Any number to the power 1 is that number itself.
%% When N is positive, X^N is equal to X times X^(N - 1)
%% When N is negative, X^N is equal to 1.0 / X^N

-spec(raise(number(), integer()) -> number()).

raise(_, 0) -> 1;

raise(X, N) when N > 0 ->
  raise(X, N, 1);

raise(X, N) when N < 0 -> 1 / raise(X, -N).

%% @doc Helper function to raise X to N by passing an Accumulator
%% from call to call.
%% When N is 0, return the value of the Accumulator;
%% otherwise return raise(X, N - 1, X * Accumulator)

raise(_, 0, Accumulator) -> Accumulator;

raise(X, N, Accumulator) ->
  raise(X, N-1, X * Accumulator).

