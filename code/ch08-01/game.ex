defmodule Game do

  def start() do
    deck = Cards.shuffle(Cards.make_deck(
#      ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
      [2, 3, 4, 5],
      ["Clubs", "Diamonds", "Hearts", "Spades"]))
    {hand1, hand2} = Enum.split(deck, trunc(Enum.count(deck) / 2))
    player1 = spawn(Player, :start, [hand1])
    player2 = spawn(Player, :start, [hand2])
    play([player1, player2], :pre_battle, [], [], 0, [])
  end

  @doc """
  Arguments are: list of player pids, state of game (atom),
  cards received from player 1, cards received from player 2,
  number of players who have given cards, and pile in middle of table
  """
  @spec play(list, atom, list, list, integer, list) :: nil
  
  def play(players, state, cards1, cards2, n_received,
    pile) do
    [player1, player2] = players
    case state do
    
      # Ask players to give you cards. If there are no
      # cards in the pile, it's a simple battle; otherwise
      # it's a war.
      
      :pre_battle ->
        IO.puts("") # for spacing
        case pile do
          [] ->
            IO.puts("Requesting 1 card from each player")
            request_cards(players, 1)
          _ ->
            IO.puts("Requesting 3 cards from each player")
            request_cards(players, 3)
        end
        play(players, :await_battle, cards1, cards2, n_received, pile)
 
      # When both players have given you their card(s),
      # you need to check them.
      
      :await_battle when n_received == 2 ->
        play(players, :check_cards, cards1, cards2, 0, pile)
      
      # Otherwise, wait for players to send you card(s). Each time
      # you receive card(s), remember them and add one to the
      # total number of players who have responded.
      
      :await_battle ->
        receive do
          {:take, new_cards, from} ->
            IO.puts("Got #{inspect(new_cards)} from #{inspect(from)}")
            cond do
              from == player1 -> play(players, state, new_cards,
                cards2, n_received + 1, pile)
              from == player2 -> play(players, state, cards1,
                new_cards, n_received + 1, pile)
            end
        end
      
      # If both people have run out of cards, it's a draw.
      # If one person is out of cards, the other player is the winner.
      # Otherwise, evaluate the cards and prepare for next
      # battle or war.
      
      :check_cards ->
        cond do
          cards1 == [] and cards2 == [] ->
            IO.puts("Draw")
            endgame(players)
          cards1 == [] ->
            IO.puts("Player 2 wins")
            endgame(players)
          cards2 == [] ->
            IO.puts("Player 1 wins")
            endgame(players)
          true -> 
            new_pile = evaluate(players, cards1, cards2, pile)
            play(players, :pre_battle, [], [], 0, new_pile)
        end   
    end
  end
  
  @spec evaluate(list, list, list, list) :: list
  
  # Evaluate the cards from both players. If their
  # values match, add them to the pile and.
  # If they don't, the winner is told to pick up the cards
  # (and whatever's in the pile), and the pile is cleared.
  #
  # Wait for players to respond before proceeding with the
  # game. Otherwise, a player with an empty hand might be
  # asked to give a card before picking up the cards she won.
  
  defp evaluate(players, cards1, cards2, pile) do
    [player1, player2] = players
    v1 = card_value(hd(cards1))
    v2 = card_value(hd(cards2))
    IO.puts("Value of card 1 is #{v1}; value of card 2 is #{v2}")
    new_pile = List.concat([pile, cards1, cards2])
    IO.puts("Card pile is now #{inspect(new_pile)}")
    cond do
      v1 == v2 ->
        IO.puts("Equal values; going to war.")
        new_pile  # it's a war
      v1 > v2 ->
        IO.puts("Telling player 1 to pick up the cards")
        player1 <- {:pick_up, new_pile, self()}
        wait_for_pickup()
        []
      v2 > v1 ->
        IO.puts("Telling player 2 to pick up the cards")
        player2 <- {:pick_up, new_pile, self()}
        wait_for_pickup()
        []
    end
  end
  
  # Wait for player to pick up cards
  @spec wait_for_pickup() :: pid
  def wait_for_pickup() do
    receive do
      {:got_cards, player} ->
        IO.puts("Player #{inspect(player)} picked up cards.")
        player
    end
  end
  
  # Send each player a requst to send n cards
  @spec request_cards(list, integer) :: nil
  
  defp request_cards([p1, p2], n) do
    p1 <- {:give, n, self()}
    p2 <- {:give, n, self()}
  end
  
  # Send message to all players to exit their receive loop.
  @spec endgame(list) :: nil
  
  defp endgame(players) do
    Enum.each(players, fn(x) -> x <- :game_over end)
  end
  
  # Return the value of a card; Aces are high.
  @spec card_value(tuple) :: integer
  
  defp card_value({value, _suit}) do
    case value do
      "A" -> 14
      "K" -> 13
      "Q" -> 12
      "J" -> 11
      _ -> value
    end
  end
end
