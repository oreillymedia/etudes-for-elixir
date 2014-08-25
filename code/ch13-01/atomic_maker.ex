defmodule AtomicMaker do
  defmacro create_functions(element_list) do
    Enum.map element_list, fn {symbol, weight} ->
      quote do
        def unquote(symbol)() do
          unquote(weight)
        end  
      end  
    end  
  end
end

