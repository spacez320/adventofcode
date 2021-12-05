/*

Advent of Code 2021, Day 2

Reads a list of strings which represent directions on a two-dimensional plane.
Calculates total distance travelled.

Usage: go run 2.go
*/

package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

// Represents movement in a direction.
type movement struct {
	direction string
	distance  int
}

// Given a list of movements, return a final Cartesian position.
func cartesianDisplacement(movements []movement) (int, int) {
	// Final destination along an X axis.
	x_pos := 0
	// Final destination along a Y axis.
	y_pos := 0

	for i := 0; i < len(movements); i++ {
		switch {
		case movements[i].direction == "forward":
			x_pos += movements[i].distance
		case movements[i].direction == "down":
			y_pos += movements[i].distance
		case movements[i].direction == "up":
			y_pos -= movements[i].distance
		}
	}

	return x_pos, y_pos
}

func vectorDisplacement(movements []movement) (int, int) {
	// Aim measurement for forward movement.
	aim := 0
	// Final destination along an X axis.
	x_pos := 0
	// Final destination along a Y axis.
	y_pos := 0

	for i := 0; i < len(movements); i++ {
		switch {
		case movements[i].direction == "forward":
			x_pos += movements[i].distance
			y_pos += movements[i].distance * aim
		case movements[i].direction == "down":
			aim += movements[i].distance
		case movements[i].direction == "up":
			aim -= movements[i].distance
		}
	}

	return x_pos, y_pos
}

func main() {
	// Regular expression for matching directions.
	r := regexp.MustCompile(`(\w+) (\d+)`)

	// Array of input movements.
	movements := []movement{}

	// Read the file.
	file, _ := os.Open("2.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	// Read input in as a separate list.
	for scanner.Scan() {
		matches := r.FindStringSubmatch(scanner.Text())

		direction := matches[1]
		distance, _ := strconv.Atoi(matches[2])
		next_movement := movement{
			direction: direction,
			distance:  distance,
		}
		movements = append(movements, next_movement)
	}

	c_x_pos, c_y_pos := cartesianDisplacement(movements)
	v_x_pos, v_y_pos := vectorDisplacement(movements)

	// Print results.
	fmt.Println("Cartesian")
	fmt.Println("=========")
	fmt.Printf("Distance is: %d\n", c_x_pos)
	fmt.Printf("Depth is: %d\n", c_y_pos)
	fmt.Printf("Totaly displacement is: %d\n", c_x_pos*c_y_pos)
	fmt.Println()
	fmt.Println("Vector")
	fmt.Println("======")
	fmt.Printf("Distance is: %d\n", v_x_pos)
	fmt.Printf("Depth is: %d\n", v_y_pos)
	fmt.Printf("Totaly displacement is: %d\n", v_x_pos*v_y_pos)
}
