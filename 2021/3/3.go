/*

Advent of Code 2021, Day 3

Analyzes occurrences of bits in a series of binary strings, reduces based on
bit occurrences, and does some binary math.

Usage: go run 3.go
*/

package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

// Convert a binary to its bit-wise inverse.
func inverseBinary(binary []int) (result []int) {
	// Resultant, inverse binary.
	result = make([]int, len(binary))

	for i, bit := range binary {
		if bit == 1 {
			result[i] = 0
		} else {
			result[i] = 1
		}
	}

	return
}

// Returns a binary string representing the least occurring value in each bit.
func leastCommonBits(binaries [][]int, winningBit int) []int {
	// Invert the winning bit.
	if winningBit == 0 {
		winningBit = 1
	} else {
		winningBit = 0
	}

	return inverseBinary(mostCommonBits(binaries, winningBit))
}

// Returns a binary string representing the most occurring value in each bit.
func mostCommonBits(binaries [][]int, winningBit int) (result []int) {
	// Tracks the occurrences of bits.
	accumulator := make([]int, len(binaries[0]))
	// Resultant, MCB  binary.
	result = make([]int, len(binaries[0]))

	// Sum values in each bit position.
	for _, binary := range binaries {
		for i, bit := range binary {
			if bit == 0 {
				accumulator[i] -= 1
			} else {
				accumulator[i] += 1
			}
		}
	}

	// Determine what value each bit position should be based on sums.
	for i, sum := range accumulator {
		if sum > 0 {
			result[i] = 1
		} else if sum < 0 {
			result[i] = 0
		} else {
			result[i] = winningBit
		}
	}

	return
}

// Reduce a list of binaries by filtering via most common bits.
func reduceBitComparison(
	binaries [][]int, filterF func([][]int, int) []int, winningBit int) []int {
	// Currently filtered binaries.
	filteredBinaries := make([][]int, len(binaries))
	copy(filteredBinaries, binaries)

	for i := 0; i < len(binaries[0]); i++ {
		// Next bit-string to filter on.
		nextFilter := filterF(filteredBinaries, winningBit)
		// Next filtered list of biinaries.
		nextFilteredBinaries := [][]int{}

		// Exit the loop if we've reached the result.
		if len(filteredBinaries) == 1 {
			break
		}

		for _, binary := range filteredBinaries {
			if binary[i] == nextFilter[i] {
				nextFilteredBinaries = append(nextFilteredBinaries, binary)
			}
		}

		filteredBinaries = nextFilteredBinaries
	}

	return filteredBinaries[0]
}

// Converts binaries to a string representation from an integer array.
func stringFromIntArrayBinary(binary []int) (result string) {
	for _, bit := range binary {
		result += strconv.Itoa(bit)
	}

	return
}

// Converts binaries from a string representation to an integer array.
func stringToIntArrayBinary(binaryS string) (result []int) {
	// Resultant, MCB  binary.
	result = make([]int, len(binaryS))

	for i, bit := range binaryS {
		result[i] = int(bit - '0') // Convert Rune to Int.
	}

	return
}

// Multiplies two binaries together.
func multiplyBinaries(binaryA []int, binaryB []int) []int {
	binaryAInt, _ := strconv.ParseInt(stringFromIntArrayBinary(binaryA[:]), 2, 0)
	binaryBInt, _ := strconv.ParseInt(stringFromIntArrayBinary(binaryB[:]), 2, 0)

	return stringToIntArrayBinary(strconv.FormatInt(binaryAInt*binaryBInt, 10))
}

func main() {
	// Full list of binaries represented by integer arrays.
	input := [][]int{}

	// Read the file.
	file, _ := os.Open("3.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	// Convert input into a list of strings.
	for scanner.Scan() {
		input = append(input, stringToIntArrayBinary(scanner.Text()))
	}

	gamma := mostCommonBits(input, 0)
	epsilon := inverseBinary(gamma)
	fmt.Printf("Result binary: %s\n", stringFromIntArrayBinary(gamma))
	fmt.Printf("Inverse binary: %s\n", stringFromIntArrayBinary(epsilon))
	fmt.Printf("Multiplied value: %s\n",
		stringFromIntArrayBinary(multiplyBinaries(gamma, epsilon)))
	fmt.Println()
	oxygen_generator_rating := reduceBitComparison(input, mostCommonBits, 1)
	fmt.Printf("MCB filtered binary: %s\n",
		stringFromIntArrayBinary(oxygen_generator_rating))
	co2_scrubber_rating := reduceBitComparison(input, leastCommonBits, 0)
	fmt.Printf("LCB filtered binary: %s\n",
		stringFromIntArrayBinary(co2_scrubber_rating))
	fmt.Printf("Multiplied value: %s\n",
		stringFromIntArrayBinary(
			multiplyBinaries(oxygen_generator_rating, co2_scrubber_rating)))
}
