%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc How not to create lists (and a correct example)
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(bad_code).
-export([squares/1, squares2/1]).

%% @doc Attempt to square a list of numbers.
%% This is the worst approach to use, as it generates improper lists.

-spec(squares(list()) -> list()).

squares(Numbers) -> squares(Numbers, []).

squares([], Result) -> Result;

squares([H | T], Result) ->
  squares(T, [Result | H * H ]).
  
%% @doc Another attempt to square a list of numbers.
%% This is a bad approach to use; at least you get a list,
%% but it's a nested list.

-spec(squares2(list()) -> list()).

squares2(Numbers) -> squares2(Numbers, []).

squares2([], Result) -> Result;

squares2([H | T], Result) ->
  squares2(T, [Result | [H * H] ]).

