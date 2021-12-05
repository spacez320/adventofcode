/*

Advent of Code 2021, Day 1

Reads a list of numbers and returns how many times a number is followed by
a larger number, both sequentially and with a sliding window of three.

Usage: go run 1.go
*/

package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
)

// Counts increases sequentially.
func increaser(input []int) int {
	// Number of counted increases.
	increases := 0

	// Loop each line and count increasing numbers.
	prev := math.MaxInt64
	for i := 0; i < len(input); i++ {
		if input[i] > prev {
			increases += 1
		}
		prev = input[i]
	}

	return increases
}

// Counts increases as a sliding window.
func slidingWindowIncreaser(input []int) int {
	// Full list of sliding window summations.
	sliding_window_input := []int{}

	// Convert the input list into a list of sliding window summations.
	next_sliding_window_sum := 0
	for i := 0; i < len(input)-2; i++ {
		for j := 0; j < 3; j++ {
			next_sliding_window_sum += input[i+j]
		}
		sliding_window_input = append(sliding_window_input, next_sliding_window_sum)
		next_sliding_window_sum = 0
	}

	return increaser(sliding_window_input)
}

func main() {
	// Full list of numbers.
	input := []int{}

	// Read the file.
	file, _ := os.Open("1.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	// Read input in as a separate list.
	for scanner.Scan() {
		next, _ := strconv.Atoi(scanner.Text())
		input = append(input, next)
	}

	// Find increase counts.
	sequential_increases := increaser(input)
	sliding_window_increases := slidingWindowIncreaser(input)

	// Print the result.
	fmt.Printf("Sequential: %d\n", sequential_increases)
	fmt.Printf("Sliding Window: %d\n", sliding_window_increases)
}
