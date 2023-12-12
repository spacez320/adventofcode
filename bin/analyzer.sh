#!/bin/bash
#
# Executes against Advent of Code solutions to produce a markdown table of some
# statistics.

CURRENT_YEAR="2023"

_exec_erl() {
  escript "${1}.erl" &>/dev/null
}

_timer() {
  local start=
  local end=

  start=$(date +%s.%N)
  $1
  end=$(date +%s.%N)
  echo "$end - $start" | bc -l
}

main() {
  local day_i=
  local input_size=
  local solution_size=
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
  echo
}

main "${@}" >> README.md
