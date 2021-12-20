package main

import "testing"

// It finds the minimum distance index and sum.
func TestMinSumDistIndexConst(t *testing.T) {
	testPoints := []int{0, 1, 2}

	gotIndex, gotSum := MinSumDistIndexConst(testPoints)
	expectedIndex := 1
	expectedSum := 2
	if gotIndex != expectedIndex {
		t.Errorf("Input: %v Got Index: %v Expected Index: %v\n",
			testPoints, gotIndex, expectedIndex)
	}
	if gotSum != expectedSum {
		t.Errorf("Input: %v Got Sum: %v Expected Sum: %v\n",
			testPoints, gotSum, expectedSum)
	}

	testPoints = []int{2, 2, 2}

	gotIndex, gotSum = MinSumDistIndexConst(testPoints)
	expectedIndex = 2
	expectedSum = 0
	if gotIndex != expectedIndex {
		t.Errorf("Input: %v Got Index: %v Expected Index: %v\n",
			testPoints, gotIndex, expectedIndex)
	}
	if gotSum != expectedSum {
		t.Errorf("Input: %v Got Sum: %v Expected Sum: %v\n",
			testPoints, gotSum, expectedSum)
	}

	testPoints = []int{16, 1, 2, 0, 4, 2, 7, 1, 2, 14}

	gotIndex, gotSum = MinSumDistIndexConst(testPoints)
	expectedIndex = 2
	expectedSum = 37
	if gotIndex != expectedIndex {
		t.Errorf("Input: %v Got Index: %v Expected Index: %v\n",
			testPoints, gotIndex, expectedIndex)
	}
	if gotSum != expectedSum {
		t.Errorf("Input: %v Got Sum: %v Expected Sum: %v\n",
			testPoints, gotSum, expectedSum)
	}
}

// It finds the minimum distance index and sum.
func TestMinSumDistIndexLinear(t *testing.T) {
	testPoints := []int{16, 1, 2, 0, 4, 2, 7, 1, 2, 14}

	gotIndex, gotSum := MinSumDistIndexLinear(testPoints)
	expectedIndex := 5
	expectedSum := 168
	if gotIndex != expectedIndex {
		t.Errorf("Input: %v Got Index: %v Expected Index: %v\n",
			testPoints, gotIndex, expectedIndex)
	}
	if gotSum != expectedSum {
		t.Errorf("Input: %v Got Sum: %v Expected Sum: %v\n",
			testPoints, gotSum, expectedSum)
	}
}

// It finds sums of distances to an index.
func TestSumDistToIndex(t *testing.T) {
	testPoints := []int{0, 1, 1, 2, 3, 5, 8, 13}

	got := SumDistToIndex(0, testPoints, DistConst)
	expected := 33

	if got != expected {
		t.Errorf("Got: %v Expected: %v\n", got, expected)
	}

	got = SumDistToIndex(0, testPoints, DistLinear)
	expected = 153

	if got != expected {
		t.Errorf("Got: %v Expected: %v\n", got, expected)
	}
}

// It calculates absolute difference between two numbers.
func TestDistConst(t *testing.T) {
	got := DistConst(0, 10)
	expected := 10

	if got != expected {
		t.Errorf("Got: %v Expected: %v\n", got, expected)
	}
}

// It calculates absolute difference between two numbers with linearly
// increasing cost.
func TestDistLinear(t *testing.T) {
	got := DistLinear(0, 10)
	expected := 55

	if got != expected {
		t.Errorf("Got: %v Expected: %v\n", got, expected)
	}
}
