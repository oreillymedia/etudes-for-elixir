%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Play the card game "war" with two players.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(game).
-export([play_game/0, dealer/0, player/2, value/1]).

%% @doc create a dealer
play_game() ->
  spawn(game, dealer, []).

dealer() ->
  random:seed(now()),
  DealerPid = self(),
  Deck = cards:shuffle(cards:make_deck()),
  {P1Cards, P2Cards} = lists:split(trunc(length(Deck) / 2), Deck),
  io:format("About to spawn players each with ~p cards.~n",
    [trunc(length(Deck) / 2)]),
  P1 = spawn(game, player, [DealerPid, P1Cards]),
  P2 = spawn(game, player, [DealerPid, P2Cards]),
  io:format("Spawned players ~p and ~p~n", [P1, P2]),
  dealer([P1, P2], pre_battle, [], [], 0, []).

%% The dealer has to keep track of the players' process IDs,
%% the cards they have given to the dealer for comparison,
%% how many players have responded (0, 1, or 2), and the pile
%% in the middle of the table in case of a war.

dealer(Pids, State, P1Cards, P2Cards, Count, Pile) ->
  [P1, P2] = Pids,
  NCards = if
    Pile == []  -> 1;
    Pile /= [] -> 3
  end,  
  case State of
    pre_battle ->
      P1 ! {give_cards, NCards},
      P2 ! {give_cards, NCards},
      dealer(Pids, await_battle, P1Cards, P2Cards, Count, Pile);
    await_battle ->
      receive
        {accept, Pid, Data} ->
          NextCount = Count + 1,
          case Pid of
            P1 -> Next_P1Cards = Data, Next_P2Cards = P2Cards;
            P2 -> Next_P1Cards = P1Cards, Next_P2Cards = Data
          end
      end,
      if
        NextCount == 2 -> NextState = check_cards;
        NextCount /= 2 -> NextState = State
      end,
      dealer(Pids, NextState, Next_P1Cards, Next_P2Cards,
        NextCount, Pile);
    check_cards ->
      Winner = game_winner(P1Cards, P2Cards),
      case Winner of
        0 ->
          io:format("Compare ~p to ~p~n", [P1Cards, P2Cards]),
          NewPile = Pile ++ P1Cards ++ P2Cards,
          case battle_winner(P1Cards, P2Cards) of
             0 -> dealer(Pids, pre_battle, [], [], 0, NewPile);
             1 ->
              P1 ! {take_cards, NewPile},
              dealer(Pids, await_confirmation, [], [], 0, []);
             2 ->
              P2 ! {take_cards, NewPile},
              dealer(Pids, await_confirmation, [], [], 0, [])
           end;
        3 ->
          io:format("It's a draw!~n"),
          end_game(Pids);
        _ ->
          io:format("Player ~p wins~n", [Winner]),
          end_game(Pids)
      end;
    await_war->
      io:format("Awaiting war~n");
    await_confirmation ->
      io:format("Awaiting confirmation of player receiving cards~n"),
      receive
        {confirmed, _Pid, _Data} ->
        dealer(Pids, pre_battle, [], [], 0, [])
      end
  end.

end_game(Pids) ->
  lists:foreach(fun(Process) -> exit(Process, kill) end, Pids),
  io:format("Game finished.~n").

%% Do we have a winner? If both players are out of cards,
%% it's a draw. If one player is out of cards, the other is the winner.

game_winner([], []) -> 3;
game_winner([], _) -> 2;
game_winner(_, []) -> 1;
game_winner(_, _) -> 0.

battle_winner(P1Cards, P2Cards) ->
  V1 = value(hd(lists:reverse(P1Cards))),
  V2 = value(hd(lists:reverse(P2Cards))),
  Winner = if
    V1 > V2 -> 1;
    V2 > V1 -> 2;
    V1 == V2 -> 0
  end,
  io:format("Winner of ~p vs. ~p is ~p~n", [V1, V2, Winner]),
  Winner = Winner.

player(Dealer, Hand) ->
  receive
    {Command, Data} ->
      case Command of
        give_cards ->
          {ToSend, NewHand} = give_cards(Hand, Data),
          io:format("Sending ~p to ~p~n", [ToSend, Dealer]),
          Dealer!{accept, self(), ToSend};
        take_cards ->
          io:format("~p now has ~p (cards)~n", [self(),
            length(Data) + length(Hand)]),
          NewHand = Hand ++ Data,
          Dealer!{confirmed, self(), []}
      end
  end,
  player(Dealer, NewHand).

%% Player gives N cards from current Hand. N is 1 or 3,
%% depending if there is a war or not.
%% If a player is asked for 3 cards but doesn't have enough,
%% give all the cards in the hand.
%% This function returns a tuple: {[cards to send], [remaining cards in hand]}

give_cards([], _N) -> {[],[]};
give_cards([A], _N) -> {[A],[]};
give_cards([A, B], N) ->
  if
    N == 1 -> {[A], [B]};
    N == 3 -> {[A, B], []}
  end;
give_cards(Hand, N) ->
  if
    N == 1 -> {[hd(Hand)], tl(Hand)};
    N == 3 ->
      [A, B, C | Remainder] = Hand,
      {[A, B, C], Remainder}
  end.
  
%% @doc Returns the value of a card. Aces are high; K > Q > J
-spec(value({cards:card()}) -> integer()).

value({V, _Suit}) ->
  if
    is_integer(V) -> V;
    is_list(V) ->
      case hd(V) of
        $J -> 11;
        $Q -> 12;
        $K -> 13;
        $A -> 14
      end
  end.
