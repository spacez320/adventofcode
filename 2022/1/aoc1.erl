%
% Advent of Code 2021, Day 1
%
% Reads a list of groups of numbers, separated by empty lines, and finds the
% largest sum of grouped numbers.

-module(aoc1).
-export([start/0]).

% Read the file.
read_input() ->
    {ok, Binary} = file:read_file("1.txt"),
    io:format("~p~n", string:tokens(erlang:binary_to_list(Binary), "\n")),
    string:tokens(erlang:binary_to_list(Binary), "\n").

% A newline has been encountered and we need to determine the next sum.
find_largest_sum_of_groups([Next | Tail], '', NextSum, Sum) when
    NextSum > Sum
->
    find_largest_sum_of_groups(Tail, Next, 0, NextSum);
find_largest_sum_of_groups([Next | Tail], '', _, Sum) ->
    find_largest_sum_of_groups(Tail, Next, 0, Sum);
% Add the next number encountered to the next sum.
find_largest_sum_of_groups([Next | Tail], NextNum, NextSum, Sum) ->
    io:format("Reading: ~p~n", [Next]),
    find_largest_sum_of_groups(Tail, Next, NextNum + NextSum, Sum);
% The end of the list has been reached and we return the largest sum.
find_largest_sum_of_groups([], _, NextSum, Sum) when NextSum > Sum -> NextSum;
find_largest_sum_of_groups([], _, _, Sum) ->
    Sum.

% Begin the traversal of the list of numbers.
find_largest_sum_of_groups([Next | Tail]) ->
    find_largest_sum_of_groups(Tail, Next, 0, 0).

% Execute and print the result.
start() ->
    Input = lists:map(
        fun(I) ->
            case string:to_integer(I) of
                {error, _} -> 0;
                {Integer, _} -> Integer
            end
        end,
        read_input()
    ),
    find_largest_sum_of_groups(Input).
