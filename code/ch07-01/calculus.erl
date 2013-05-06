%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Find the derivative of a function Fn at point X.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(calculus).
-export([derivative/2]).

%% @doc Calculate derivative by classical definition.
%% (Fn(X + H) - Fn(X)) / H

-spec(derivative(function(), float()) -> float()).

derivative(Fn, X) ->
	Delta = 1.0e-10,
	(Fn(X + Delta) - Fn(X)) / Delta.


