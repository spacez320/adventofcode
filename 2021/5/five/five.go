package main

import (
	"fmt"
	"math"
)

type Point struct {
	visits int
	x      int
	y      int
}

type Grid struct {
	points []Point
}

// Constructor for a drid.
func NewGrid(x_size, y_size int) (grid Grid) {
	for y := 0; y < y_size; y++ {
		for x := 0; x < x_size; x++ {
			grid.points = append(grid.points, Point{visits: 0, x: x, y: y})
		}
	}

	return
}

// Return the number of points on the grid visited at least a certain amount.
func (grid Grid) countPointsByVisits(visits int) (sum int) {
	for _, point := range grid.points {
		if point.visits >= visits {
			sum += 1
		}
	}

	return
}

// Returns a specific point on the grid.
func (grid Grid) find(x, y int) (foundPoint *Point) {
	for i, point := range grid.points {
		if point.x == x && point.y == y {
			foundPoint = &grid.points[i]
			break
		}
	}

	return
}

// Pretty-prints a grid.
func (grid Grid) pprint() {
	lastY := 0
	for _, point := range grid.points {
		if point.y != lastY {
			fmt.Printf("\n")
			lastY = point.y
		}

		switch point.visits {
		case 0:
			fmt.Printf(".")
		default:
			fmt.Printf("%d", point.visits)
		}
	}

	fmt.Println()
}

// Visits points on a grid in straight lines or 45deg lines.
func (grid Grid) traverse(start, end Point) (distance int) {
	if start.y == end.y {
		// This is a horizontal traversal.
		endX := maxInt(start.x, end.x)
		startX := minInt(start.x, end.x)
		for x := startX; x <= endX; x++ {
			grid.visit(x, start.y)
			distance += 1
		}
	} else if start.x == end.x {
		// This is a vertical traversal.
		endY := maxInt(start.y, end.y)
		startY := minInt(start.y, end.y)
		for y := startY; y <= endY; y++ {
			grid.visit(start.x, y)
			distance += 1
		}
	} else {
		// This is a diagonal line.
		x := start.x
		y := start.y
		for i := 0; i <= int(math.Abs(float64(start.x-end.x))); i++ {
			grid.visit(x, y)
			distance += 1

			// Determining the next position depends on the positivity or negativity
			// of the slope.
			if end.x > start.x {
				x++
			} else {
				x--
			}
			if end.y > start.y {
				y++
			} else {
				y--
			}
		}
	}

	return
}

// Visits a point on a grid.
func (grid Grid) visit(x, y int) {
	var point *Point
	point = grid.find(x, y)
	point.visits += 1
}

// Find a maximum integer.
func maxInt(x, y int) int {
	if x >= y {
		return x
	} else {
		return y
	}
}

// Find a minimum integer.
func minInt(x, y int) int {
	if x < y {
		return x
	} else {
		return y
	}
}
