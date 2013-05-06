%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for splitting a date into a list of
%% year-month-day.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(dates).
-export([date_parts/1]).

%% @doc Takes a string in ISO date format (yyyy-mm-dd) and
%% returns a list of integers in form [year, month, day].

-spec(date_parts(list()) -> list()).

date_parts(DateStr) ->
  [YStr, MStr, DStr] = re:split(DateStr, "-", [{return, list}]),
  [element(1, string:to_integer(YStr)),
    element(1, string:to_integer(MStr)),
    element(1, string:to_integer(DStr))].
