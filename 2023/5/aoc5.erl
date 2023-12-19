%
% Adven of Code 2023, Day 5
%
% Given a series of maps that transform a starting category value into an
% ending one, finds the lowest resultant category value from a set of starting
% category values.

-module(aoc5).
-include_lib("eunit/include/eunit.hrl").
-export([main/1]).

% Reads input from a file.
%
% Returns:
%   list: Read input.
read_input() ->
    {ok, Input} = file:read_file("5.txt"),
    string:tokens(binary_to_list(Input), "\n").

% Given a map entry, produce a tuple that represents the source-to-destination
% map and the destinations to map to.
%
% E.g.
%
%   [1, 2, 3] -> {{2, 5}, {1, 4}}
%
make_map_and_destination_ranges(SourceDestinationMap) ->
    DestinationRangeStart = lists:nth(1, SourceDestinationMap),
    MapRangeStart = lists:nth(2, SourceDestinationMap),
    RangeLength = lists:nth(3, SourceDestinationMap),
    {
        {MapRangeStart, MapRangeStart + RangeLength},
        {DestinationRangeStart, DestinationRangeStart + RangeLength}
    }.

% Given a list of sources, produce a list of tuples which represent source
% ranges.
%
% E.g.
%
%   [1, 2, 3, 4] -> [{1, 3}, {3, 7}]
%
make_source_ranges(Sources) ->
    make_source_ranges(Sources, []).
make_source_ranges([], SourceRanges) ->
    SourceRanges;
make_source_ranges(Sources, SourceRanges) ->
    make_source_ranges(
        lists:sublist(Sources, 3, length(Sources)),
        lists:append(SourceRanges, [
            {lists:nth(1, Sources), lists:nth(1, Sources) + lists:nth(2, Sources)}
        ])
    ).

% Determines a destination number from a source given a single mapping.
get_destination(Source, SourceDestinationMap) when is_integer(Source) ->
    get_destination(
        Source,
        lists:nth(2, SourceDestinationMap),
        lists:nth(1, SourceDestinationMap),
        lists:nth(3, SourceDestinationMap)
    );
% Redirect to the ranged functions when source is a range.
get_destination(Source, SourceDestinationMap) when is_tuple(Source) ->
    get_destination_from_range({Source, Source}, SourceDestinationMap).
% This function indicates the source number is outside the source range and
% should be returned as the destination number.
get_destination(Source, MapRangeStart, _, RangeLength) when
    Source < MapRangeStart;
    Source > (MapRangeStart + RangeLength - 1)
->
    Source;
% This clause retrieves the destination mapped to the source.
get_destination(Source, MapRangeStart, DestinationRangeStart, _) ->
    DestinationRangeStart + (Source - MapRangeStart).

% Given a source range and a source-to-destination map, produce a destination value.
get_destination_from_range(SourceRange, SourceDestinationMap) ->
    {MapRange, DestinationRange} = make_map_and_destination_ranges(SourceDestinationMap),
    get_destination_from_range(SourceRange, MapRange, DestinationRange).
% The source range is entirely outside the mapping range. In this case there is
% no destination to map to and we should return the first element of the source
% range as the destination.
get_destination_from_range(SourceRange, MapRange, _) when
    element(1, SourceRange) > element(2, MapRange);
    element(2, SourceRange) < element(1, MapRange)
->
    element(1, SourceRange);
% The source range and mapping range intersect, but the source range starts
% before the mapping range. Therefore, the first indexed destination is
% returned.
get_destination_from_range(SourceRange, MapRange, DestinationRange) when
    element(1, SourceRange) < element(1, MapRange)
->
    element(1, DestinationRange);
% Otherwise figure out the index where the source range starts within the map
% range and return the corresponding destination.
get_destination_from_range(SourceRange, MapRange, DestinationRange) ->
    element(1, DestinationRange) + element(1, SourceRange) - element(1, MapRange).

% Determines a destination number from a source from multiple mappings.
get_destination_from_maps(Source, [Head | Tail]) ->
    get_destination_from_maps(get_destination_from_maps(Source, Head, Source), Tail);
get_destination_from_maps(Destination, []) ->
    Destination.
% We found a mapping and should stop early and return it.
get_destination_from_maps(Source, _, Destination) when Source /= Destination ->
    Destination;
% We should continue searching for a destination.
get_destination_from_maps(Source, [Head | Tail], _) ->
    get_destination_from_maps(Source, Tail, get_destination(Source, Head));
% We've considered all mappings and should just return the current destination.
get_destination_from_maps(_, [], Destination) ->
    Destination.

% Iterates through a list of sources, maps them to their resultant destination,
% and returns the minimum destination.
find_min_destination(Sources, SourceDestinationMaps) ->
    lists:min(
        lists:map(
            fun(Source) -> get_destination_from_maps(Source, SourceDestinationMaps) end,
            Sources
        )
    ).

% Iterates through a list of source ranges, maps them to their resultant
% destination, and returns the minimum destination.
find_min_destination_ranges(Sources, SourceDestinationMaps) ->
    find_min_destination_ranges(Sources, SourceDestinationMaps, 0).
% Considers the next source range.
find_min_destination_ranges([Head | Tail], SourceDestinationMaps, CurrentMinDestination) ->
    find_min_destination_ranges(
        Tail,
        SourceDestinationMaps,
        case CurrentMinDestination of
            0 ->
                find_min_destination_ranges(
                    element(1, Head), element(2, Head), SourceDestinationMaps, 0
                );
            _Else ->
                min(
                    find_min_destination_ranges(
                        element(1, Head), element(2, Head), SourceDestinationMaps, 0
                    ),
                    CurrentMinDestination
                )
        end
    );
% All source changes have been considered.
find_min_destination_ranges([], _, CurrentMinDestination) ->
    CurrentMinDestination.
% Iterate through the next source offered from the source range.
find_min_destination_ranges(
    Source, SourceRangeEnd, SourceDestinationMaps, CurrentMinDestination
) when
    Source /= SourceRangeEnd
->
    find_min_destination_ranges(
        Source + 1,
        SourceRangeEnd,
        SourceDestinationMaps,
        case CurrentMinDestination of
            0 ->
                get_destination_from_maps(Source, SourceDestinationMaps);
            _Else ->
                min(
                    get_destination_from_maps(Source, SourceDestinationMaps),
                    CurrentMinDestination
                )
        end
    );
% All sources in the source range have been considered.
find_min_destination_ranges(_, _, _, CurrentMinDestination) ->
    CurrentMinDestination.

% Reads the problem input. It should return a list with the following structure:
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
parse_input(Input) ->
    parse_input(Input, []).
% Considers the next line of input to parse.
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
% Considers the next mapping to add to the next list of mappings.
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

% Entry-point.
main(_) ->
    ParsedInput = parse_input(read_input()),
    io:format("Min destination: ~p~n", [
        find_min_destination(
            lists:nth(1, ParsedInput), lists:sublist(ParsedInput, 2, length(ParsedInput))
        )
    ]),

    io:format("Min destination (with ranges): ~p~n", [
        find_min_destination_ranges(
            make_source_ranges(lists:nth(1, ParsedInput)),
            lists:sublist(ParsedInput, 2, length(ParsedInput))
        )
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% TESTS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% It creates a tuple of mapping and destination ranges.
make_map_and_destination_ranges_test() ->
    {{1, 2}, {3, 4}} = make_map_and_destination_ranges([3, 1, 1]).

% It creates a list of source range tuples.
make_source_ranges_test() ->
    [{1, 3}, {3, 7}] = make_source_ranges([1, 2, 3, 4]).

% It returns the source value if outside the bounds.
get_destination_gt_test() -> 2 = get_destination(2, 1, 1, 1).
get_destination_lt_test() -> 1 = get_destination(1, 2, 2, 2).
% It returns the correct index value.
get_destination_one_test() -> 3 = get_destination(1, 1, 3, 1).
get_destination_two_test() -> 4 = get_destination(2, 1, 3, 2).

% Sanity check test case.
get_destination_from_range_a_test() -> 1 = get_destination_from_range({1, 1}, {1, 1}, {1, 1}).
% It returns the first element of the source range when outside the bounds of the mapping.
get_destination_from_range_b_test() ->
    5 = get_destination_from_range({5, 8}, {1, 2}, {1, 1}),
    5 = get_destination_from_range({5, 8}, {9, 10}, {1, 1}).
% It returns the correct destination when the mapping range starts before the source range.
get_destination_from_range_c_test() -> 2 = get_destination_from_range({5, 8}, {4, 7}, {1, 2}).

% It finds a mapping that exists.
get_destination_from_maps_found_test() ->
    10 = get_destination_from_maps(5, [[[1, 1, 1], [10, 5, 1]]]).
% It resturns a source number with no mapping.
get_destination_from_maps_not_found_test() ->
    5 = get_destination_from_maps(5, [[[1, 1, 1], [10, 6, 1]]]).

% It returns a minimum destination map from a set of number sources.
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

% It returns a minimum destination map from a set of ranged sources.
find_min_destination_ranges_test() ->
    46 = find_min_destination_ranges(
        [{79, 79 + 14}, {55, 55 + 13}],
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
