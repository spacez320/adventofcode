package main

import (
	"regexp"
	"strconv"
)

// Converts a string of delimited numbers into an array of integers.
func ReadNumsInput(input string) (output []int) {
	// Match each number found and apply it to the result.
	r := regexp.MustCompile(`\d+`)
	for _, match := range r.FindAllString(input, -1) {
		next, _ := strconv.Atoi(match)
		output = append(output, next)
	}

	return
}
