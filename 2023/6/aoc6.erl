%
% Adven of Code 2023, Day 6

-module(aoc6).
-include_lib("eunit/include/eunit.hrl").
-export([main/1]).

% Reads input from a file.
read_input() ->
    {ok, Input} = file:read_file("6.txt"),
    string:tokens(binary_to_list(Input), "\n").

% Provides tuples of time and distances from input.
parse_input(Input) ->
    lists:zip(parse_input_line(lists:nth(1, Input)), parse_input_line(lists:nth(2, Input))).

% Provides a list of numbers from a line of input.
parse_input_line(InputLine) ->
    {match, Match} = re:run(
        lists:nth(2, string:split(InputLine, " ")), "\\d+", [global, {capture, all, list}]
    ),
    lists:map(fun(S) -> element(1, string:to_integer(S)) end, Match).

% Like parse_input but returns only a single tuple.
parse_input_single(Input) ->
    lists:zip(
        parse_input_line_single(lists:nth(1, Input)), parse_input_line_single(lists:nth(2, Input))
    ).

% Like parse_input_line but combines all numbers into a single number.
parse_input_line_single(InputLine) ->
    [
        element(
            1,
            string:to_integer(
                string:replace(lists:nth(2, string:split(InputLine, " ")), " ", "", all)
            )
        )
    ].

% Performs a binary search exhaustively.
%
% Account for base cases with two remaining elements.
exhaustive_binary_search(Pred, [Last1, Last2]) ->
    case Pred(Last1) of
        true -> Last1;
        _ -> Last2
    end;
% Account for base cases with one remaining element.
exhaustive_binary_search(_, [Last]) ->
    Last;
% Perform the next search.
exhaustive_binary_search(Pred, List) ->
    NextIndex = round(length(List) / 2),

    case Pred(lists:nth(NextIndex, List)) of
        % We found a match, so consider the former segment. in search of non-matches
        true -> exhaustive_binary_search(Pred, lists:sublist(List, NextIndex));
        % We found a non-match, so consider the latter segment in search of matches.
        _ -> exhaustive_binary_search(Pred, lists:sublist(List, NextIndex, length(List)))
    end.

% Like the above, but returns the index rather than the value.
exhaustive_binary_search_i(Pred, List) ->
    element(1, exhaustive_binary_search(fun(I) -> Pred(element(2, I)) end, lists:enumerate(List))).

% Determines whether a race was won.
is_winning_race(Distance, Rate, Time, ButtonTime) ->
    (Distance / Rate) < (Time * ButtonTime) - math:pow(ButtonTime, 2).

% Determines the minimum amount of time the button needs to be pressed in order
% to win a game.
find_minimum_winning_button_time(Distance, Rate, Time) ->
    exhaustive_binary_search(
        fun(I) -> is_winning_race(Distance, Rate, Time, I) end, lists:seq(1, round(Time / 2))
    ).

% Finds the total number of winning button times.
sum_winning_button_times(Distance, Rate, Time) ->
    Median = round(Time / 2),
    MinimumWinningButtonTime = find_minimum_winning_button_time(Distance, Rate, Time),

    case Time band 1 == 0 of
        true ->
            % This is an odd distance and we need to account for a middle
            % element.
            2 * (Median - MinimumWinningButtonTime + 1) - 1;
        _ ->
            2 * (Median - MinimumWinningButtonTime)
    end.

% Finds the product of the count of winning inputs.
product_of_winning_button_times(Inputs) ->
    product_of_winning_button_times(Inputs, 1).
product_of_winning_button_times([Head | Tail], Product) ->
    NextDistance = element(2, Head),
    NextTime = element(1, Head),

    product_of_winning_button_times(
        Tail,
        Product * (sum_winning_button_times(NextDistance, 1, NextTime))
    );
product_of_winning_button_times([], Product) ->
    Product.

% Entry-point.
main(_) ->
    io:format("Answer for separate races: ~p~n", [
        product_of_winning_button_times(parse_input(read_input()))
    ]),

    io:format(
        "Answer for single race: ~p~n",
        [product_of_winning_button_times(parse_input_single(read_input()))]
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% TESTS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_input_test() -> [{1, 2}, {3, 4}, {5, 6}] = parse_input(["Foo: 1 3 5", "Bar: 2 4 6"]).

parse_input_single_test() -> [{135, 246}] = parse_input_single(["Foo: 1 3 5", "Bar: 2 4 6"]).

parse_input_line_test() -> [1, 22, 333] = parse_input_line("Test: 1 22 333").

parse_input_line_single_test() -> [122333] = parse_input_line_single("Test: 1   22   333").

exhaustive_binary_search_test() ->
    2 = exhaustive_binary_search(fun(I) -> I >= 2 end, [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]).

exhaustive_binary_search_i_test() ->
    3 = exhaustive_binary_search_i(fun(I) -> I >= 2 end, [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]).

is_winning_time_test() ->
    false = is_winning_race(9, 1, 7, 0),
    true = is_winning_race(9, 1, 7, 3).

find_minimum_winning_button_time_test() -> 2 = find_minimum_winning_button_time(9, 1, 7).

sum_winning_button_times_test() ->
    4 = sum_winning_button_times(9, 1, 7),
    8 = sum_winning_button_times(40, 1, 15),
    29 = sum_winning_button_times(283, 1, 44),
    9 = sum_winning_button_times(200, 1, 30).

product_of_winning_button_times_test() ->
    288 = product_of_winning_button_times([{7, 9}, {15, 40}, {30, 200}]).
