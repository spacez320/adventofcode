%
% Advent of Code 2023, Day 1
%
% Reads a series of strings containing at least one number and produces a sum
% of each string's first and last numbers combined together into a two digit
% number.


-module(aoc1).
-export([start/0]).


% Reads input from a file.
read_input() ->
  {ok, Input} = file:read_file("1.txt"),
  string:tokens(erlang:binary_to_list(Input), "\n").


% Parses a discovered number, which may be a digit (as a string) or a word.
parse_next_number(N) ->
  case N of
    "one" -> 1;
    "two" -> 2;
    "three" -> 3;
    "four" -> 4;
    "five" -> 5;
    "six" -> 6;
    "seven" -> 7;
    "eight" -> 8;
    "nine" -> 9;
    _  -> element(1, string:to_integer(N))
  end.


% Finds the next number in a string, which may be a digit or a word. In the
% event that no number is found, zero is returned.
find_number(S, R) ->
  {ok, NumberRegex} = re:compile(R),
  case re:run(S, NumberRegex, [{capture, all, list}]) of
    {match, Capture} -> parse_next_number(lists:nth(2, Capture));
    nomatch -> 0
  end.


% Finds the left-most number in a string.
find_left_number(S) ->
  find_number(S, "(one|two|three|four|five|six|seven|eight|nine|\\d)").


% Finds the right-most number in a string.
find_right_number(S) ->
  find_number(S, ".*(one|two|three|four|five|six|seven|eight|nine|\\d)").


main([Head|Tail], Sum) ->
  FirstNumber = find_left_number(Head),
  SecondNumber = find_right_number(Head),

  % Return an updated sum. The first number is multiplied by 10 to become the
  % first digit in the two digit number.
  main(Tail, Sum + (FirstNumber * 10) + SecondNumber);

main([], Sum) -> Sum.


start() -> main(read_input(), 0).
