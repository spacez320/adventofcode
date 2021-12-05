%
%
% Advent of Code 2021, Day 2
% 
% Reads a list of strings which represent directions on a two-dimensional
% plane. Calculates total distance travelled.
%
% Usage:
%
%   erl
%   erl> c(aoc2).
%   aoc2:start().

-module(aoc2).
-export([start/0]).

% Read the file and convert to a list of directions and distances.
read_input() ->
  {ok, Binary} = file:read_file("2.txt"),
  lists:map(fun(I) ->
                case re:run(I, "(\\w+) (\\d+)", [{capture, all, list}]) of
                  {match, Capture} -> {
                            lists:nth(2, Capture),
                            element(
                              1, string:to_integer(lists:nth(3, Capture)))}
                end
            end,  string:tokens(erlang:binary_to_list(Binary), "\n")).

cartesian_displacement(Movements) ->
  cartesian_displacement(Movements, 0, 0).
cartesian_displacement([NextMovement|Movements], X, Y) ->
  case NextMovement of
    {"forward", Distance} ->
      cartesian_displacement(Movements, X + Distance, Y);
    {"down", Distance} -> cartesian_displacement(Movements, X, Y + Distance);
    {"up", Distance} -> cartesian_displacement(Movements, X, Y - Distance)
  end;
cartesian_displacement([], X, Y) -> X * Y.

vector_displacement(Movements) ->
  vector_displacement(Movements, 0, 0, 0).
vector_displacement([NextMovement|Movements], X, Y, Aim) ->
  case NextMovement of
    {"forward", Distance} ->
      vector_displacement(Movements, X + Distance, Y + Aim * Distance, Aim);
    {"down", Distance} -> vector_displacement(Movements, X, Y, Aim + Distance);
    {"up", Distance} -> vector_displacement(Movements, X, Y, Aim - Distance)
  end;
vector_displacement([], X, Y, _) -> X * Y.

% Execute and print the result.
start() -> 
  Input = read_input(),
  io:fwrite(
    "Cartesian displacement: ~p~n", [cartesian_displacement(Input)]),
  io:fwrite(
    "Vector displacement: ~p~n", [vector_displacement(Input)]).
