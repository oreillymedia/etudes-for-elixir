%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for playing a card game.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(cards).
-export([make_deck/0, show_deck/1]).

%% @doc generate a deck of cards
make_deck() ->
  [{Value, Suit} || Value <- ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
    Suit <- ["Clubs", "Diamonds", "Hearts", "Spades"]].

show_deck(Deck) ->
  lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).
  
