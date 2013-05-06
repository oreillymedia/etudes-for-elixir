-module(person_test).
-export([start/0, listener/0, talker/1]).

start()->
  Pid1 = spawn(node(),person_test,listener,[]),
  io:format("Pid is ~p~n", [Pid1]),
  talker(Pid1).

listener() ->
	receive
		X -> io:format("Got ~p~n", [X])
	end.

talker(Pid) ->
  Input = io:get_line("Msg > "),
  Pid ! {ok, Input},
  talker(Pid).

