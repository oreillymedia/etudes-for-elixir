defmodule Player do
  def start(hand) do
    play(hand)
  end
  
  #
  # The player can either be told to give the dealer
  # n cards (1 or 3), pick up cards (after having won a battle),
  # or leave the game.
  
  def play(hand) do
    receive do
      {:give, n, dealer} ->
        {to_send, to_keep} = Enum.split(hand, n)
        dealer <- {:take, to_send, self()}
        play(to_keep)
      {:pick_up, cards, dealer} ->
        new_hand = hand ++ cards
        IO.puts("Player #{inspect(self)} has #{inspect(new_hand)}")
        dealer <- {:got_cards, self()}
        play(new_hand)
      :game_over -> IO.puts("Player #{inspect(self)} leaves.")
    end
  end
end
