%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Use pattern matching in a list comprehension.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(patmatch).
-export([older_males/0, older_or_male/0]).

%% @doc Select all males older than 40 from a list of tuples giving
%% name, gender, and age.

-spec(older_males() -> list()).

get_people() ->
  [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30},
  {"Vu", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}].

older_males() ->
  People = get_people(),
  [Name || {Name, Gender, Age} <- People, Gender == $M, Age > 40].

older_or_male() ->
  People = get_people(),
  [Name || {Name, Gender, Age} <- People, (Gender == $M) orelse (Age > 40)].

