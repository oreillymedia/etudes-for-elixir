%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions to calculate areas of shape given user input.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(ask_area).
-export([area/0]).

%% @doc Requests a character for the name of a shape,
%% numbers for its dimensions, and calculates shape's area.
%% The characters are R for rectangle, T for triangle,
%% and E for ellipse. Input is allowed in either upper
%% or lower case. 

-spec(area() -> number()).

area() ->
  Answer = io:get_line("R)ectangle, T)riangle, or E)llipse > "),
  Shape = char_to_shape(hd(Answer)),
  case Shape of
    rectangle -> Numbers = get_dimensions("width", "height");
    triangle -> Numbers = get_dimensions("base", "height");
    ellipse -> Numbers = get_dimensions("major axis", "minor axis");
    unknown -> Numbers = {error, "Unknown shape " ++ [hd(Answer)]}
  end,
  
  Area = calculate(Shape, element(1, Numbers), element(2, Numbers)),
  Area.

%% @doc Given a character, returns an atom representing the
%% specified shape (or the atom unknown if a bad character is given).

-spec(char_to_shape(char()) -> atom()).

char_to_shape(Char) ->
  case Char of
    $R -> rectangle;
    $r -> rectangle;
    $T -> triangle;
    $t -> triangle;
    $E -> ellipse;
    $e -> ellipse;
    _ ->  unknown
  end.

%% @doc Present a prompt and get a number from the
%% user. Allow either integers or floats.

-spec(get_number(string()) -> number()).

get_number(Prompt) ->
  Str = io:get_line("Enter " ++ Prompt ++ " > "),
  {Test, _} = string:to_float(Str),
  case Test of
    error -> {N, _} = string:to_integer(Str);
    _ -> N = Test
  end,
  N.

%% @doc Get dimensions for a shape. Input are the two prompts,
%% output is a tuple {Dimension1, Dimension2}.

-spec(get_dimensions(string(), string()) -> {number(), number()}).

get_dimensions(Prompt1, Prompt2) ->
  N1 = get_number(Prompt1),
  N2 = get_number(Prompt2),
  {N1, N2}.

%% @doc Calculate area of a shape, given its shape and dimensions.
%% Handle errors appropriately.

-spec(calculate(atom(), number(), number()) -> number()).

calculate(unknown, _, Err) -> io:format("~s~n", [Err]);
calculate(_, error, _) -> io:format("Error in first number.~n");
calculate(_, _, error) -> io:format("Error in second number.~n");
calculate(_, A, B) when A < 0; B < 0 ->
  io:format("Both numbers must be greater than or equal to zero~n");
calculate(Shape, A, B) -> geom:area(Shape, A, B).



