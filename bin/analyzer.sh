#!/bin/bash
#
# Executes against Advent of Code solutions to produce a markdown table of some
# statistics.
#
# Usage:
#
# bin/analyzer.sh >> README.md

# Year we are analyzing (probably the current one).
CURRENT_YEAR="2023"

# Executes an AOC solution. Assumes an Erlang script with an exported function
# signature of `main/1`.
_exec_erl() {
  escript "${1}.erl" &>/dev/null
}

# Times the execution of a script.
_timer() {
  # Beginning time.
  local start=
  # Ending time.
  local end=

  start=$(date +%s.%N)
  $1
  end=$(date +%s.%N)

  echo "$end - $start" | bc -l
}

main() {
  # Next day index for the table.
  local day_i=
  # Next day's input size (in bytes).
  local input_size=
  # Next day's solution size (in bytes).
  local solution_size=
  # Next day's solution execution time (in seconds).
  local solution_time=

  pushd $CURRENT_YEAR &>/dev/null || exit

  echo -en """
| Day | Input Size (B) | Solution Size (B) | Solution Time (s) |
| --- | -------------- | ----------------- | ----------------- |"""

  day_i=1
  for day in *; do
    pushd "${day}" &>/dev/null || continue

    input_size=$(wc --bytes "${day}.txt" | awk '{print $1}')
    solution_size=$(wc --bytes "aoc${day}.erl" | awk '{print $1}')
    solution_time=$(_timer "_exec_erl \"aoc${day}\"")

    echo -en """
| ${day_i} | ${input_size} | ${solution_size} | ${solution_time} |"""

    day_i=$(( day_i + 1))
    popd &>/dev/null || continue
  done

  # One more newline at the end.
  echo
}

main "${@}"
