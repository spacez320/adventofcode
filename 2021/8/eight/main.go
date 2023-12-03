/*

Advent of code 2021, Day 8

Decodes seven digit displays.

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
	input := adventUtil.ReadSubPatternf("8.txt", `([\w ]+)`)
	fmt.Println(input)
}
