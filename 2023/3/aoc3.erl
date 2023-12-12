%
% Advent of Code 2023, Day 3
%
% Given a gird of symbols and numbers, find numbers which neighbor those
% symbols and produce a sum of them.

-module(aoc3).
-export([main/1]).

% Reads input from a file.
read_input() ->
    {ok, Input} = file:read_file("3.txt"),
    string:tokens(erlang:binary_to_list(Input), "\n").

% Seeks out number neighbors of a specific point (presumably a symbol) and sums
% together any that are found.
%
% This is the init function.
sum_number_neighbors(Search, NumberMap) ->
    % Displacements for a point to search all neighboring points.
    NeighborOffsets = [{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}],
    sum_number_neighbors(NeighborOffsets, [], Search, NumberMap, 0).

% Iterates through searchable neighbor offsets for a new number.
sum_number_neighbors([Head | Tail], Present, Search, NumberMap, Sum) ->
    % Point to search next.
    NextSearch = {element(1, Head) + element(1, Search), element(2, Head) + element(2, Search)},

    case maps:find(NextSearch, NumberMap) of
        {ok, Found} ->
            case lists:member(element(2, Found), Present) of
                true ->
                    % We've already considered this number and should just skip it.
                    sum_number_neighbors(Tail, Present, Search, NumberMap, Sum);
                false ->
                    % This number seems unaccounted for.
                    sum_number_neighbors(
                        Tail,
                        lists:append(Present, [element(2, Found)]),
                        Search,
                        NumberMap,
                        Sum + element(1, string:to_integer(element(1, Found)))
                    )
            end;
        error ->
            % This point is not present in the number map.
            sum_number_neighbors(Tail, Present, Search, NumberMap, Sum)
    end;
% All searchable neighbor offsets have been inspected.
sum_number_neighbors([], _, _, _, Sum) ->
    Sum.

% Seeks out number neighbors of a specific point (presumably a symbol) and sums
% together their products if that point has strictly two numbers neighbors.
%
% This is the init function.
product_duo_number_neighbors(Search, NumberMap) ->
    % Displacements for a point to search all neighboring points.
    NeighborOffsets = [{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}],

    product_duo_number_neighbors(NeighborOffsets, [], Search, NumberMap).

% Iterates through searchable neighbor offsets for a new number (as long as we
% haven't already discovered two).
product_duo_number_neighbors([Head | Tail], Present, Search, NumberMap) when
    length(Present) =< 2
->
    % Point to search next.
    NextSearch = {element(1, Head) + element(1, Search), element(2, Head) + element(2, Search)},

    case maps:find(NextSearch, NumberMap) of
        {ok, Found} ->
            case lists:member(Found, Present) of
                true ->
                    % We've already considered this number and should just skip it.
                    product_duo_number_neighbors(Tail, Present, Search, NumberMap);
                false ->
                    % This number seems unaccounted for.
                    product_duo_number_neighbors(
                        Tail, lists:append(Present, [Found]), Search, NumberMap
                    )
            end;
        error ->
            % This point is not present in the number map.
            product_duo_number_neighbors(Tail, Present, Search, NumberMap)
    end;
% Stop things early if we find more than two numbers.
product_duo_number_neighbors(_, Present, _, _) when length(Present) > 2 -> 0;
% We've got exactly two numbers and should return their product
product_duo_number_neighbors([], Present, _, _) when length(Present) == 2 ->
    element(
        2,
        lists:mapfoldl(
            fun(X, Product) -> {X, element(1, string:to_integer(element(1, X))) * Product} end,
            1,
            Present
        )
    );
% Otherwise we return zero.
product_duo_number_neighbors([], _, _, _) ->
    0.

% Adds entries to a number map.
%
% This pattern considers the next entry.
put_number_map([Head | Tail], Input, Index, NumberMap) ->
    % Index in Input where the value appears.
    NextValueIndex = element(1, Head),
    % Length of the number value.
    NextValueLength = element(2, Head),
    % Retrieve the captured value from input. Note that substring searches are
    % 1-indexed.
    NextValue = string:substr(Input, NextValueIndex + 1, NextValueLength),

    % Place the value into the number map.
    put_number_map(
        Tail,
        Input,
        Index,
        put_number_map_i(
            Index, NextValue, NextValueIndex, NextValueLength, {Index, NextValueIndex}, NumberMap
        )
    );
% This pattern has exhausted entries to add.
put_number_map([], _, _, NumberMap) ->
    NumberMap.

% Iterator function for adding an entry to the number map. Numbers added to the
% number map may be multiple digits and span multiple coordinates on the grid.
%
% This clause adds an additional coordinate within an entry to the number map.
put_number_map_i(Index, Value, ValueIndex, ValueLength, ValueStart, NumberMap) when
    ValueLength /= 0
->
    put_number_map_i(
        Index,
        Value,
        ValueIndex + 1,
        ValueLength - 1,
        ValueStart,
        maps:put({Index, ValueIndex}, {Value, ValueStart}, NumberMap)
    );
% This pattern is finished entering coordinates for an entry.
put_number_map_i(_, _, _, _, _, NumberMap) ->
    NumberMap.

% Init function for building the symbol list.
make_number_map(S) -> make_number_map(S, 0, #{}).

% Builds the number map data structure.
%
% Entries in the number map has the following structure:
%
%   {X, Y} => N
%
% where {X, Y} is a coordinate which holds the number and N is the number.
%
% This pattern scans input for more entries.
make_number_map([Head | Tail], Index, NumberMap) ->
    case re:run(Head, "[\\d]+", [global]) of
        {match, Capture} ->
            make_number_map(
                Tail,
                Index + 1,
                put_number_map(lists:flatten(Capture), Head, Index, NumberMap)
            );
        nomatch ->
            make_number_map(Tail, Index + 1, NumberMap)
    end;
% This pattern has finished all scans.
make_number_map([], _, NumberMap) ->
    NumberMap.

% Adds entries to a symbol list.
%
% This pattern considers the next entry.
put_symbol_list([Head | Tail], Index, SymbolList) ->
    NextSymbolIndex = element(1, Head),
    put_symbol_list(Tail, Index, lists:append(SymbolList, [{Index, NextSymbolIndex}]));
% This pattern has exhausted all entries.
put_symbol_list([], _, SymbolList) ->
    SymbolList.

% Init function for building the symbol list.
make_symbol_list(S) -> make_symbol_list(S, 0, []).

% Builds the symbol map data structure.
%
% The map is a list of tuples which have the following structure:
%
%   {I, [{X1, Y1}, {X1, Y2}}
%
% where I is the row where a symbol is found and X is the column where the
% symbol is found.
make_symbol_list([Head | Tail], Index, SymbolList) ->
    case re:run(Head, "[^\\d\.]", [global]) of
        {match, Capture} ->
            make_symbol_list(
                Tail, Index + 1, put_symbol_list(lists:flatten(Capture), Index, SymbolList)
            );
        nomatch ->
            make_symbol_list(Tail, Index + 1, SymbolList)
    end;
% This pattern has exhausted all entries.
make_symbol_list([], _, SymbolList) ->
    SymbolList.

% Entrypoint for summing numbers next to symbols.
sum_all_number_neighbors(SymbolMap, NumberMap) ->
    sum_all_number_neighbors(SymbolMap, NumberMap, 0).

% Iterator for summing numbers next to symbols.
sum_all_number_neighbors([Head | Tail], NumberMap, Sum) ->
    sum_all_number_neighbors(Tail, NumberMap, Sum + sum_number_neighbors(Head, NumberMap));
% All numbers are summed.
sum_all_number_neighbors([], _, Sum) ->
    Sum.

% Entrypoint for summing duo number neighbor products.
sum_all_product_duo_number_neighbors(SymbolMap, NumberMap) ->
    sum_all_product_duo_number_neighbors(SymbolMap, NumberMap, 0).

% Iterator for summing duo number neighbor products.
sum_all_product_duo_number_neighbors([Head | Tail], NumberMap, Sum) ->
    sum_all_product_duo_number_neighbors(
        Tail, NumberMap, Sum + product_duo_number_neighbors(Head, NumberMap)
    );
% All numbers are summed.
sum_all_product_duo_number_neighbors([], _, Sum) ->
    Sum.

main(_) ->
    Input = read_input(),

    SymbolList = make_symbol_list(Input),
    NumberMap = make_number_map(Input),

    io:format("Sum of symbol neighbors: ~p~n", [sum_all_number_neighbors(SymbolList, NumberMap)]),
    io:format("Sum of duo-neighbor products: ~p~n", [
        sum_all_product_duo_number_neighbors(SymbolList, NumberMap)
    ]).
