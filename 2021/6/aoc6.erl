%
% Advent of Code 2021, Day 6
%
% Simulates population growth.
%
% Usage:
%
%   erl
%   erl> c(aoc6).
%   aoc6:start().

-module(aoc6).
-export([start/0]).


% Read the input file.
read_input() ->
  {ok, Binary} = file:read_file("6.txt"),
  lists:map(fun(I) ->
                case string:to_integer(I) of
                  {error, _} -> 0;
                  {Integer, _} -> Integer
                end
            end,
            string:tokens(erlang:binary_to_list(Binary), ",")).


% Entrypoint.
epoch(Population, Length) -> epoch(Population, 0, Length).

% Return the population when the last moment is reached.
epoch(Population, Moments, Length) when Moments == Length -> Population;

% Process the epoch and iterate.
epoch(Population, Moments, Length) ->
  io:format("Beginning Epoch: ~p~n", [Moments]),
  epoch(moment(Population), Moments + 1, Length).


% Entrypoint.
moment(Population) -> moment(Population, []).

% Reset the head individual to 6 and append a new individual to the current
% population.
moment([One|Many], NextEpochMany) when One == 0 ->
  moment(Many, NextEpochMany ++ [6, 8]);

% Iterate to the next moment and reduce the head individual by 1.
moment([One|Many], NextEpochMany) ->
  moment(Many, NextEpochMany ++ [One-1]);

% Return the population as is when done processing the moment.
moment([], NextEpochMany) ->
  NextEpochMany.


% Execute and print the result.
start() ->
  Input = read_input(),
  length(epoch(Input, 80)).
