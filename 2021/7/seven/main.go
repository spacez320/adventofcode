/*

Advent of code 2021, Day 7

Finds the minimum of a function of horizontal points against their relative
distance.

Usage:
	go mod tidy
	go run main.go

*/

package main

import (
	"fmt"

	"github.com/spacez320/adventUtil"
)

func main() {
	// Read the file.
	input := adventUtil.ReadIntegersCommaf("7.txt")[0]

	// Find the minimum distance index.
	minIndex, minSum := MinSumDistIndexConst(input)
	fmt.Printf("Minimum index: %v Minimum distance: %v\n", minIndex, minSum)

	// Find the minimum distance index with linear cost increase.
	minIndex, minSum = MinSumDistIndexLinear(input)
	fmt.Printf("Minimum index: %v Minimum distance: %v\n", minIndex, minSum)
}
