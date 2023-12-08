%
% Advent of Code 2023, Day 2
%
% 1.  Reads a series of strings containing information about ball draw games,
%     finding which ones are possible and then summing their ids.
%
% 2.  Determines the maximum color draws per game, multiplies them, and
%     produces the sum of these products.

-module(aoc2).
-export([start/0]).

% Reads input from a file.
read_input() ->
    {ok, Input} = file:read_file("2.txt"),
    string:tokens(erlang:binary_to_list(Input), "\n").

% Given a color entry match, determine whether that draw violates the available
% colors in the game.
%
% A color entry match should be a list that looks something like this:
%
%   ["1 blue", "1"]
%
check_color_validity([Head | Tail], ColorLimit) ->
    (element(1, string:to_integer(lists:nth(2, Head))) =< ColorLimit) and
        check_color_validity(Tail, ColorLimit);
check_color_validity([], _) ->
    true.

% Finds color entries with their associated numbers within a game.
find_color(S, Color) ->
    {ok, Regex} = re:compile(lists:flatten(io_lib:format("(\\d+) ~p", [Color]))),
    case re:run(S, Regex, [global, {capture, all, list}]) of
        {match, Capture} -> Capture;
        nomatch -> nomatch
    end.

% Finds a game id.
find_game_id(S) ->
    {ok, Regex} = re:compile("^Game (\\d+)"),
    case re:run(S, Regex, [{capture, all, list}]) of
        {match, Capture} -> element(1, string:to_integer(lists:nth(2, Capture)));
        nomatch -> nomatch
    end.

% Finds the largest draw in a set of color draws.
%
% A color entry match should be a list that looks something like this:
%
%   ["1 blue", "1"]
%
find_largest_color([Head | Tail], CurrentLargest) ->
    Next = element(1, string:to_integer(lists:nth(2, Head))),
    case Next > CurrentLargest of
        true -> find_largest_color(Tail, Next);
        false -> find_largest_color(Tail, CurrentLargest)
    end;
find_largest_color([], CurrentLargest) ->
    CurrentLargest.

% Finds sums of game ids for valid games.
sum_valid_games([Head | Tail], Sum) ->
    % Limits for color draws.
    ColorLimits = #{red => 12, blue => 14, green => 13},

    % Isolate the game id.
    GameId = find_game_id(Head),

    % Verify red.
    RedColors = find_color(Head, red),
    RedValid = check_color_validity(RedColors, maps:get(red, ColorLimits)),

    % Verify blue.
    BlueColors = find_color(Head, blue),
    BlueValid = check_color_validity(BlueColors, maps:get(blue, ColorLimits)),

    % Verify green.
    GreenColors = find_color(Head, green),
    GreenValid = check_color_validity(GreenColors, maps:get(green, ColorLimits)),

    case RedValid and BlueValid and GreenValid of
        true -> sum_valid_games(Tail, Sum + GameId);
        false -> sum_valid_games(Tail, Sum)
    end;
sum_valid_games([], Sum) ->
    Sum.

% Finds the sume of products of largest colors in games.
sum_largest_color_products([Head | Tail], Sum) ->
    % Find largest red.
    RedColors = find_color(Head, red),
    LargestRed = find_largest_color(RedColors, 0),

    % Find largest blue.
    BlueColors = find_color(Head, blue),
    LargestBlue = find_largest_color(BlueColors, 0),

    % Find largest green.
    GreenColors = find_color(Head, green),
    LargestGreen = find_largest_color(GreenColors, 0),

    sum_largest_color_products(Tail, Sum + (LargestRed * LargestBlue * LargestGreen));
sum_largest_color_products([], Sum) ->
    Sum.

start() ->
    % Actual input.
    Input = read_input(),

    % Part 1.
    sum_valid_games(Input, 0),
    % Part 2.
    sum_largest_color_products(Input, 0).
