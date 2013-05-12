defmodule GenerateCalls do
  @moduledoc """ 
  Generate a random set of phone calls
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  def make_call_list(n) do
    now = :calendar.datetime_to_gregorian_seconds({{2013, 3, 10},
      {9, 0, 0}})
    numbers = [
      {"213-555-0172", now},
      {"301-555-0433", now},
      {"415-555-7871", now},
      {"650-555-3326", now},
      {"729-555-8855", now},
      {"838-555-1099", now},
      {"946-555-9760", now}
    ]
    call_list = make_call_list(n, numbers, [])
    {result, output_file} = File.open("call_list.csv", [:write,
      :utf8])
    case result do
      :ok -> write_item(output_file, call_list)
      :error -> IO.puts("Error creating output file")
    end
  end
  
  def make_call_list(0, _numbers, result) do
    Enum.reverse(result)
  end
  
  def make_call_list(n, numbers, result) do
    entry = :random.uniform(Enum.count(numbers))
    {head, tail} = Enum.split(numbers, entry - 1)
    {number, last_call} = hd(tail)
    start_call = last_call + :random.uniform(120) + 20
    duration = :random.uniform(180) + 40
    end_call = start_call + duration
    item = [number, format_date(start_call), format_time(start_call),
      format_date(end_call), format_time(end_call)]
    updated_numbers = head ++ [{number, end_call} | tl(tail)]
    make_call_list(n - 1, updated_numbers, [item | result])
  end
  
  def write_item(output_file, []) do
    File.close(output_file)
  end
  
  def write_item(output_file, [h | t]) do
    [n, sd, st, ed, et] = h
    IO.write(output_file, "#{n},#{sd},#{st},#{ed},#{et}\n")
    write_item(output_file, t)
  end
  
  def format_date(g_seconds) do
    {{y, m, d}, _time} =
      :calendar.gregorian_seconds_to_datetime(g_seconds)
    "#{y}-#{leadzero(m)}-#{leadzero(d)}"
  end
  
  def format_time(g_seconds) do
    {_date, {h, m, s}} =
      :calendar.gregorian_seconds_to_datetime(g_seconds)
    "#{leadzero(h)}:#{leadzero(m)}:#{leadzero(s)}"
  end
  
  def leadzero(n) do
    cond do
      n < 10 -> "0" <> integer_to_binary(n)
      true -> integer_to_binary(n)
    end
  end
end
