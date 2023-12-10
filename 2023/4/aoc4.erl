%
% Advent of Code 2023, Day 4

-module(aoc4).
-export([start/0]).

% Reads input from a file.
read_input() ->
    {ok, Input} = file:read_file("4.txt"),
    string:tokens(erlang:binary_to_list(Input), "\n").

get_candidate_numbers(S) ->
    string:tokens(
        lists:nth(2, element(2, re:run(S, "\\| ([\\d ]+)", [{capture, all, list}]))), " "
    ).

get_winning_numbers(S) ->
    string:tokens(
        lists:nth(2, element(2, re:run(S, ": ([\\d ]+)\\|", [{capture, all, list}]))), " "
    ).

sum_points(CandidateNumbers, WinningNumbers) ->
    lists:foldl(
        fun(_, Sum) ->
            case Sum of
                0 -> 1;
                _Else -> Sum * 2
            end
        end,
        0,
        [X || X <- CandidateNumbers, lists:member(X, WinningNumbers)]
    ).

sum_all_points(Input) ->
    sum_all_points(Input, 0).

sum_all_points([Head | Tail], Sum) ->
    sum_all_points(Tail, Sum + sum_points(get_candidate_numbers(Head), get_winning_numbers(Head)));
sum_all_points([], Sum) ->
    Sum.

start() ->
    Input = read_input(),

    io:format("Sum from winning numbers: ~p~n", [sum_all_points(Input)]).
