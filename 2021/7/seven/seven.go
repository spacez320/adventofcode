package main

import (
	"math"
)

// Returns the index that represents the minimum total distance between each
// point in a set of points. Do this by finding the last number to decrease.
func MinSumDistIndexConst(points []int) (minIndex, minSum int) {
	var nextSum int
	lastSum := SumDistToIndex(0, points, DistConst)
	minIndex = len(points) - 1
	for i := 1; i < len(points); i++ {
		nextSum = SumDistToIndex(i, points, DistConst)
		if lastSum <= nextSum {
			minIndex = i - 1
			minSum = lastSum
			break
		}
		lastSum = nextSum
	}

	return
}

// Returns the index that represents the minimum total distance between each
// point in a set of points, given that cost of travel between points linearly
// increases. Do this by finding the last number to decrease.
func MinSumDistIndexLinear(points []int) (minIndex, minSum int) {
	var nextSum int
	lastSum := SumDistToIndex(0, points, DistLinear)
	minIndex = len(points) - 1
	for i := 1; i < len(points); i++ {
		nextSum = SumDistToIndex(i, points, DistLinear)
		if lastSum <= nextSum {
			minIndex = i - 1
			minSum = lastSum
			break
		}
		lastSum = nextSum
	}

	return
}

// Calculates the total distance a series of points has to an index.
func SumDistToIndex(
	index int, points []int, incf func(int, int) int) (sumDist int) {
	for _, point := range points {
		sumDist += incf(point, index)
	}

	return
}

// Function for returning a constant distance between two points.
func DistConst(a, b int) int {
	return int(math.Abs(float64(a - b)))
}

// Function for returning a distance with linearly increasing cost between two
// points.
func DistLinear(a, b int) int {
	absDist := DistConst(a, b)
	return int((absDist * (absDist + 1)) / 2)
}
