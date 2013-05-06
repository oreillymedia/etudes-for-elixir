%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Show teeth that need attention due to excessive pocket depth.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(teeth).
-export([alert/1]).

%% @doc Create a list of tooth numbers that require attention.

-spec(alert[integer()]) -> [integer()]).

alert(ToothList) -> alert(ToothList, 1, []).

%% @doc Helper function that accumulates the list of teeth needing attention

-spec(alert([integer()], integer(), [integer()]) -> [integer()]).

alert([], _Tooth_number, Result) -> lists:reverse(Result);

alert([Head | Tail ], ToothNumber, Result ) ->
  case stats:maximum(Head) >= 4 of
    true -> alert(Tail, ToothNumber + 1, [ToothNumber | Result]);
    false -> alert(Tail, ToothNumber + 1, Result)
  end.

