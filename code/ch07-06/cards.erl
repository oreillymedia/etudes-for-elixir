%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for playing a card game.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(cards).
-export([make_deck/0, shuffle/1]).

%% @doc generate a deck of cards
make_deck() ->
  [{Value, Suit} || Value <- ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
    Suit <- ["Clubs", "Diamonds", "Hearts", "Spades"]].
    
shuffle(List) -> shuffle(List, []).

%% If the list is empty, return the accumulated value.
shuffle([], Acc) -> Acc;

%% Otherwise, find a random location in the list and split the list
%% at that location. Let's say the list has 52 elements and the random
%% location is location 22. The first 22 elements go into Leading, and the
%% last 30 elements go into [H|T]. Thus, H would contain element 23, and
%% T would contain elements 24 through 52.
%%
%% H is the "chosen element". It goes into the accumulator (the shuffled list)
%% and then we call shuffle again with the remainder of the deck: the 
%% leading elements and the tail of the split list.

shuffle(List, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).

