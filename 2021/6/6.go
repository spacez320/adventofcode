/*

Advent of Code 2021, Day 6

Simulates population growth.

Usage: go run 6.go
*/

package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

const (
	EPOCHS_TO_SIMULATE       = 256 // Number of epochs to simulate.
	NEW_POPULATION_VALUE     = 8   // Value for new pops.
	REFRESH_POPULATION_VALUE = 6   // Reset value for pops after breeding.
)

// Simulates one population cycle.
func epoch(
	populationCounter [NEW_POPULATION_VALUE + 1]int) (
	newPopulationCounter [NEW_POPULATION_VALUE + 1]int) {
	for i, populationCount := range populationCounter {
		switch i {
		case 0:
		case REFRESH_POPULATION_VALUE + 1:
			newPopulationCounter[i-1] = populationCounter[0] + populationCount
		case NEW_POPULATION_VALUE:
			newPopulationCounter[i-1] = populationCount
			newPopulationCounter[i] = populationCounter[0]
		default:
			newPopulationCounter[i-1] = populationCount
		}
	}

	return
}

func main() {
	// Full list of initial population.
	population := []int{}
	// Array of counters for populations states.
	populationCounter := [NEW_POPULATION_VALUE + 1]int{}
	// Array of counters for populations states.
	var populationSum int

	// Read the file.
	input, _ := ioutil.ReadFile("6.txt")
	for _, num := range strings.SplitN(string(input), ",", -1) {
		nextNum, _ := strconv.Atoi(strings.TrimSpace(num))
		population = append(population, nextNum)
	}

	// Populate the initial set of counters.
	for _, next := range population {
		populationCounter[next] += 1
	}

	// Begin epochs.
	for i := 0; i < EPOCHS_TO_SIMULATE; i++ {
		fmt.Printf("Beginning Epoch: %v\n", i)
		populationCounter = epoch(populationCounter)
	}

	// Produce the final sum.
	for _, num := range populationCounter {
		populationSum += num
	}

	// Print the result.
	fmt.Printf("%d\n", populationSum)
}
