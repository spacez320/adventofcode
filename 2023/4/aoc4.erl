%
% Advent of Code 2023, Day 4
%
% Considers a series of lottery cards and produces a number based on the number
% of winning numbers.

-module(aoc4).
-export([main/1]).

% Reads input from a file.
read_input() ->
    {ok, Input} = file:read_file("4.txt"),
    string:tokens(erlang:binary_to_list(Input), "\n").

% Parses candidate numbers on a lottery card.
get_candidate_numbers(S) ->
    string:tokens(
        lists:nth(2, element(2, re:run(S, "\\| ([\\d ]+)", [{capture, all, list}]))), " "
    ).

% Parses winning numbers on a lottery card.
get_winning_numbers(S) ->
    string:tokens(
        lists:nth(2, element(2, re:run(S, ": ([\\d ]+)\\|", [{capture, all, list}]))), " "
    ).

% Creates the card copy list for calculating total card points.
%
% It should return a data structure that looks like this:
%
% [
%   [X1, X2, ..., XN],
%   [Y1, Y2, ..., YN],
%   ...
% ]
%
% where each entry is a sequence of numbers generated from matching winning
% numbers.
%
% This is the init function.
make_card_copy_list(Input) ->
    make_card_copy_list(Input, 1, []).

% This iterates through a card to add an entry to the card copy list.
make_card_copy_list([Head | Tail], Index, CardCopyList) ->
    make_card_copy_list(
        Tail,
        Index + 1,
        put_card_copy_list(
            Index, get_candidate_numbers(Head), get_winning_numbers(Head), CardCopyList
        )
    );
% This returns the finaly card copy list.
make_card_copy_list([], _, CardCopyList) ->
    CardCopyList.

% Places a new entry into a card copy list.
put_card_copy_list(Index, CandidateNumbers, WinningNumbers, CardCopyList) ->
    lists:append(
        CardCopyList,
        [
            lists:seq(
                Index + 1,
                Index + length([X || X <- CandidateNumbers, lists:member(X, WinningNumbers)])
            )
        ]
    ).

% Finds a total sum of cards, including their copies.
sum_cards(CardCopySumMap) ->
    maps:fold(
        fun(_, V, Sum) -> V + Sum end,
        0,
        CardCopySumMap
    ).

% Creates the card copy sum map for calculating total numbers of cards.
%
% It should return a data structure that looks like this:
%
% #{
%   I => N,
%   ...
% }
%
% where I is a card index and N is the sum of cards for that index. To be
% efficient, it reverses the input to try to calculate latter cards first so as
% to not have to re-calculate them when they appear later as copies.
%
% This is the init function.
make_card_copy_sum_list(CardCopyList) ->
    make_card_copy_sum_list(lists:reverse(lists:seq(1, length(CardCopyList))), CardCopyList, #{}).

% This function iterates on cards to find a card sum and apply it to the map.
make_card_copy_sum_list([Head | Tail], CardCopyList, CardCopySumMap) ->
    make_card_copy_sum_list(
        Tail,
        CardCopyList,
        put_card_copy_sum_list(
            lists:nth(Head, CardCopyList),
            Head,
            CardCopyList,
            CardCopySumMap,
            0
        )
    );
% We've considered all cards and should return the finished map.
make_card_copy_sum_list([], _, CardCopySumMap) ->
    CardCopySumMap.

% Places a new entry into the card copy sum map. It tries to be efficient by
% checking the map for already computed sums.
put_card_copy_sum_list([Head | Tail], Index, CardCopyList, CardCopySumMap, Sum) ->
    case maps:find(Head, CardCopySumMap) of
        {ok, Value} ->
            put_card_copy_sum_list(Tail, Index, CardCopyList, CardCopySumMap, Sum + Value);
        error ->
            NextCardCopies = lists:nth(Head, CardCopyList),
            put_card_copy_sum_list(
                lists:append(Tail, NextCardCopies), Index, CardCopyList, CardCopySumMap, Sum + 1
            )
    end;
% We are ready to add an entry to the card copy sum map.
put_card_copy_sum_list([], Index, _, CardCopySumMap, Sum) ->
    maps:put(Index, Sum + 1, CardCopySumMap).

% Finds a power of 2 based on valid winning numbers for a lottery card.
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

% Reads serveral lottery cards and sums the results.
%
% This is the init function.
sum_all_points(Input) ->
    sum_all_points(Input, 0).

% Iterates through lottery cards to find another sum-able result.
sum_all_points([Head | Tail], Sum) ->
    sum_all_points(Tail, Sum + sum_points(get_candidate_numbers(Head), get_winning_numbers(Head)));
% All lottery cards have been considered.
sum_all_points([], Sum) ->
    Sum.

main(_) ->
    % Read input.
    Input = read_input(),

    % Part 1.
    io:format("Sum from winning numbers: ~p~n", [sum_all_points(Input)]),
    % Part 2.
    io:format("Sum from copied cards: ~p~n", [
        sum_cards(make_card_copy_sum_list(make_card_copy_list(Input)))
    ]).
