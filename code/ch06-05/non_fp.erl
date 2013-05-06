%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Generate a random set of teeth, with a certain
%% percentage expected to be bad.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(non_fp).
-export([generate_teeth/2, test_teeth/0]).

%% @doc Generate a list of lists, six numbers per tooth, giving random
%% pocket depths. Takes a string where T="there's a tooth there"
%% and F="no tooth"), and a float giving probability that a tooth is good.

-spec(generate_teeth(string(), float()) -> list(list(integer()))).

generate_teeth(TeethPresent, ProbGood) ->
  random:seed(now()),
  generate_teeth(TeethPresent, ProbGood, []).

%% @doc Helper function that adds tooth data to the ultimate result.

-spec(generate_teeth(string(), float(), [[integer()]]) -> [[integer()]]).

generate_teeth([], _Prob, Result) -> lists:reverse(Result);

generate_teeth([$F | Tail], ProbGood, Result) ->
  generate_teeth(Tail, ProbGood, [[0] | Result]);
  
generate_teeth([$T | Tail], ProbGood, Result) ->
  generate_teeth(Tail, ProbGood,
  [generate_tooth(ProbGood) | Result]).

-spec(generate_tooth(float()) -> list(integer())).

%% @doc Generates a list of six numbers for a single tooth. Choose a
%% random number between 0 and 1. If that number is less than the probability
%% of a good tooth, it sets the "base depth" to 2, otherwise it sets the base
%% depth to 3.

generate_tooth(ProbGood) ->
  Good = random:uniform() < ProbGood,
  case Good of
    true -> BaseDepth = 2;
    false -> BaseDepth = 3
  end,
  generate_tooth(BaseDepth, 6, []).

%% @doc Take the base depth, add a number in range -1..1 to it,
%% and add it to the list.

generate_tooth(_Base, 0, Result) -> Result;

generate_tooth(Base, N, Result) ->
  [Base + random:uniform(3) - 2 | generate_tooth(Base, N - 1, Result)].
  
test_teeth() ->
  TList = "FTTTTTTTTTTTTTTFTTTTTTTTTTTTTTTT",
  N = generate_teeth(TList, 0.75),
  print_tooth(N).

print_tooth([]) -> io:format("Finished.~n");
print_tooth([H|T]) ->
  io:format("~p~n", [H]),
  print_tooth(T).
  
  
