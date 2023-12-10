%
% Advent of Code 2021, Day 1
%
% Reads a list of numbers and returns how many times a number is followed by a
% larger number, both sequentially and with a sliding window of three.

-module(aoc1).
-export([start/0]).

% Read the file.
read_input() ->
    {ok, Binary} = file:read_file("1.txt"),
    string:tokens(erlang:binary_to_list(Binary), "\n").

% Count the number of increasing sequential numbers.
increaser([Next | Tail]) -> increaser(Tail, Next, 0).
increaser([Next | Tail], Prev, Increases) when Next > Prev ->
    increaser(Tail, Next, Increases + 1);
increaser([Next | Tail], _, Increases) ->
    increaser(Tail, Next, Increases);
increaser([], _, Increases) ->
    Increases.

% Count the number of increasing numbers based on a sliding window of three.
sliding_window_increaser([Next1, Next2, Next3 | Tail]) ->
    sliding_window_increaser([Next2, Next3 | Tail], Next1 + Next2 + Next3, 0).
sliding_window_increaser([Next1, Next2, Next3 | Tail], Prev, Increases) when
    Next1 + Next2 + Next3 > Prev
->
    sliding_window_increaser(
        [Next2, Next3 | Tail], Next1 + Next2 + Next3, Increases + 1
    );
sliding_window_increaser([Next1, Next2, Next3 | Tail], _, Increases) ->
    sliding_window_increaser(
        [Next2, Next3 | Tail], Next1 + Next2 + Next3, Increases
    );
sliding_window_increaser(_, _, Increases) ->
    Increases.

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
    increaser(Input),
    sliding_window_increaser(Input).
