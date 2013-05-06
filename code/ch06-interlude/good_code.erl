%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Correct example of how to create lists.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(good_code).
-export([correct_squares/1]).

%% @doc Successfully square a list of numbers.
%% This is the correct approach to use. When you use | to construct
%% a list, the new item always comes at the head, and the old list is last.

-spec(correct_squares(list()) -> list()).

correct_squares(Numbers) -> correct_squares(Numbers, []).

correct_squares([], Result) -> lists:reverse(Result);

correct_squares([H | T], Result) ->
  correct_squares(T, [H * H | Result]).
  
