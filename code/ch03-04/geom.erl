%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(geom).
-export([area/1]).

%% @doc Calculates the area of a shape, given a tuple
%% containing a shape and two of the dimensions.
%% Works by calling a private function.

-spec(area({atom(), number(),number()}) -> number()).

area({Shape, Dim1, Dim2}) -> area(Shape, Dim1, Dim2).

%% @doc Returns the product of its arguments for a rectangle,
%% one half the product of the arguments for a triangle,
%% and math:pi times the product of the arguments for
%% an ellipse. Invalid data returns zero.

-spec(area(atom(), number(),number()) -> number()).

area(rectangle, L,W) when L >=0, W >= 0 -> L * W;

area(triangle, B, H) when B>= 0, H >= 0 -> (B * H) / 2.0;

area(ellipse, A, B) when A >= 0, B >= 0 -> math:pi() * A * B;

area(_, _, _) -> 0.

