%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for raising a number to an integer power
%% and finding the Nth root of a number using Newton's method.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(powers).
-export([raise/2]).

%% @doc Raise a number X to an integer power N.
%% Any number to the power 0 equals 1.
%% Any number to the power 1 is that number itself.
%% When N is positive, X^N is equal to X times X^(N - 1)
%% When N is negative, X^N is equal to 1.0 / X^N

-spec(raise(number(), integer()) -> number()).

raise(_, 0) -> 1;

raise(X, 1) -> X;

raise(X, N) when N > 0 -> X * raise(X, N - 1);

raise(X, N) when N < 0 -> 1 / raise(X, -N).

