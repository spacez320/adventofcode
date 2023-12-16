%
% Adven of Code 2023, Day 5
%
% - Given a series of maps that transform a starting category into an ending
%   one, finds the lowest resultant category value from a set of starting
%   category values.

-module(aoc5).
-include_lib("eunit/include/eunit.hrl").
-export([main/1]).

% Reads input from a file.
read_input() ->
    {ok, Input} = file:read_file("5.txt"),
    string:tokens(binary_to_list(Input), "\n").

% Determines a destination number given a source number and source and
% destination ranges.
%
% This is the init function.
get_destination(Source, SourceDestinationMap) ->
    get_destination(
        Source,
        lists:nth(2, SourceDestinationMap),
        lists:nth(1, SourceDestinationMap),
        lists:nth(3, SourceDestinationMap)
    ).
% This clause indicates the source number is outside the source range and
% should be returned as the destination number.
get_destination(Source, SourceRangeStart, _, RangeLength) when
    Source < SourceRangeStart; Source > (SourceRangeStart + RangeLength - 1)
->
    Source;
% This clause retrieves the destination mapped to the source.
get_destination(Source, SourceRangeStart, DestinationRangeStart, _RangeLength) ->
    % Subtract by one because lists are 1-indexed.
    DestinationRangeStart + (Source - SourceRangeStart).

% Given a source, produces a destination based on a series of maps.
get_destination_from_map(Source, SourceDestinationMap) ->
    get_destination_from_map(Source, SourceDestinationMap, Source).
% We found a mapping and can return it.
get_destination_from_map(Source, _, Destination) when Source /= Destination ->
    Destination;
% We should continue searching for a destination.
get_destination_from_map(Source, [Head | Tail], _) ->
    get_destination_from_map(Source, Tail, get_destination(Source, Head));
% We've considered all mappings.
get_destination_from_map(_, [], Destination) ->
    Destination.

% Iterates through a series of mappings to find a resultant destination.
get_destination_from_maps(Source, [Head | Tail]) ->
    get_destination_from_maps(get_destination_from_map(Source, Head), Tail);
get_destination_from_maps(Destination, []) ->
    Destination.

% Iterates through a series of sources, maps them to their resultant
% destination, and returns the minimum.
find_min_destination(Sources, SourceDestinationMaps) ->
    lists:min(
        lists:map(
            fun(Source) -> get_destination_from_maps(Source, SourceDestinationMaps) end,
            Sources
        )
    ).

% Reads the problem input. It should return a tuple with the following structure:
%
% [
%   [S1, S2, ...],
%   [[M1, M2, M3], ...],
%   ...
%   [[MN, MN+1, MN+2], ...],
% ]
%
% where:
%
% - [S1, S2, ...] is the starting source set.
% - [M1, M2, M3] is a mapping entry.
%
% This is the init clause.
parse_input(Input) ->
    parse_input(Input, []).
% This clause explicitly handles the starting source list.
parse_input([Head | Tail], Inputs) ->
    case re:run(Head, "\\d+", [global, {capture, all, list}]) of
        {match, Match} ->
            % This is the first entry of numbers.
            parse_input(
                Tail,
                lists:append(
                    Inputs,
                    [lists:flatmap(fun(X) -> [element(1, string:to_integer(X))] end, Match)]
                )
            );
        nomatch ->
            % This is the first mapping category header.
            parse_input(Tail, [], Inputs)
    end.
% This clause considers the next mapping to add to the next series of mappings.
parse_input([Head | Tail], NextInput, Inputs) ->
    case re:run(Head, "\\d+", [global, {capture, all, list}]) of
        {match, Match} ->
            % This is an entry of numbers.
            parse_input(
                Tail,
                lists:append(
                    NextInput,
                    [lists:flatmap(fun(X) -> [element(1, string:to_integer(X))] end, Match)]
                ),
                Inputs
            );
        nomatch ->
            % This must be a category start string.
            parse_input(Tail, [], lists:append(Inputs, [NextInput]))
    end;
% All input has been considered.
parse_input([], NextInput, Inputs) ->
    lists:append(Inputs, [NextInput]).

main(_) ->
    ParsedInput = parse_input(read_input()),
    find_min_destination(
        lists:nth(1, ParsedInput), lists:sublist(ParsedInput, 2, length(ParsedInput))
    ).

% It returns the source value if outside the bounds.
get_destination_gt_test() -> 2 = get_destination(2, 1, 1, 1).
get_destination_lt_test() -> 1 = get_destination(1, 2, 2, 2).
% It returns the correct index value.
get_destination_one_test() -> 3 = get_destination(1, 1, 3, 1).
get_destination_two_test() -> 4 = get_destination(2, 1, 3, 2).

% It finds a mapping that exists.
get_destination_from_map_found_test() -> 10 = get_destination_from_map(5, [[1, 1, 1], [10, 5, 1]]).
% It resturns a source number with no mapping.
get_destination_from_map_not_found_test() ->
    5 = get_destination_from_map(5, [[1, 1, 1], [10, 6, 1]]).

% It navigates a series of mappings to a destination.
get_destination_from_maps_found_test() ->
    10 = get_destination_from_maps(1, [[[5, 1, 1]], [[10, 5, 1]]]).
% It returns a source number if there is a break in mappings.
get_destination_from_maps_not_found_test() ->
    5 = get_destination_from_maps(1, [[[5, 1, 1]], [[10, 6, 1]]]).

% It returns a minimum destination map from a set of sources.
find_min_destination_test() ->
    35 = find_min_destination(
        [79, 14, 55, 13],
        [
            [[50, 98, 2], [52, 50, 48]],
            [[0, 15, 37], [37, 52, 2], [39, 0, 15]],
            [[49, 53, 8], [0, 11, 42], [42, 0, 7], [57, 7, 4]],
            [[88, 18, 7], [18, 25, 70]],
            [[45, 77, 23], [81, 45, 19], [68, 64, 13]],
            [[0, 69, 1], [1, 0, 69]],
            [[60, 56, 37], [56, 93, 4]]
        ]
    ).

% It parses input correctly.
parse_input_test() ->
    [
        [79, 14, 55, 13],
        [[50, 98, 2], [52, 50, 48]],
        [[0, 15, 37], [37, 52, 2], [39, 0, 15]],
        [[49, 53, 8], [0, 11, 42], [42, 0, 7], [57, 7, 4]],
        [[88, 18, 7], [18, 25, 70]],
        [[45, 77, 23], [81, 45, 19], [68, 64, 13]],
        [[0, 69, 1], [1, 0, 69]],
        [[60, 56, 37], [56, 93, 4]]
    ] = parse_input(
        [
            "seeds: 79 14 55 13",
            "seed-to-soil map:",
            "50 98 2",
            "52 50 48",
            "soil-to-fertilizer map:",
            "0 15 37",
            "37 52 2",
            "39 0 15",
            "fertilizer-to-water map:",
            "49 53 8",
            "0 11 42",
            "42 0 7",
            "57 7 4",
            "water-to-light map:",
            "88 18 7",
            "18 25 70",
            "light-to-temperature map:",
            "45 77 23",
            "81 45 19",
            "68 64 13",
            "temperature-to-humidity map:",
            "0 69 1",
            "1 0 69",
            "humidity-to-location map:",
            "60 56 37",
            "56 93 4"
        ]
    ).
