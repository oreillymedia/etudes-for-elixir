defmodule Duration do
    defmacro add({a,b},{aa,bb}) do
        mm = round((b+bb)/60)
        {a+aa+mm,b+bb-mm*60}
    end
end
