%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for splitting a date into a list of
%% year-month-day and finding Julian date.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(dates).
-export([date_parts/1, julian/1, is_leap_year/1]).

%% @doc Takes a string in ISO date format (yyyy-mm-dd) and
%% returns a list of integers in form [year, month, day].

-spec(date_parts(list()) -> list()).

date_parts(DateStr) ->
  [YStr, MStr, DStr] = re:split(DateStr, "-", [{return, list}]),
  [element(1, string:to_integer(YStr)),
    element(1, string:to_integer(MStr)),
    element(1, string:to_integer(DStr))].

%% @doc Takes a string in ISO date format (yyyy-mm-dd) and
%% returns the day of the year (Julian date).
%% Works by summing the days per month up to, but not including,
%% the month in question, then adding the number of days.
%% If it's a leap year and past February, add a leap day.

-spec(julian(list()) -> integer()).

julian(DateStr) ->
  DaysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
  [Y, M, D] = date_parts(DateStr),
  {Sublist, _} = lists:split(M - 1, DaysPerMonth),
  Total = lists:foldl(fun(V, A) -> V + A end, 0, Sublist),
  case M > 2 andalso is_leap_year(Y) of 
    true -> Total + D + 1;
    false -> Total + D
  end.  

is_leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
    orelse (Year rem 400 == 0).
