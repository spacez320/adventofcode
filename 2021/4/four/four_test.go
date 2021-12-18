package main

import "testing"

// It returns bingo from a marked out list of numbers.
func TestBingoNumsCheckBingo(t *testing.T) {
	testBingoNum := BingoNums{
		nums: []int{1, 2, 3},
	}

	if testBingoNum.CheckBingo() {
		t.Errorf("Erroneous bingo with testBingoNum: %v\n",
			testBingoNum)
	}

	testBingoNum = BingoNums{
		nums: []int{-1, -1, -1},
	}

	if !testBingoNum.CheckBingo() {
		t.Errorf("Erroneous non-bingo with testBingoNum: %v\n",
			testBingoNum)
	}
}

// It copies a set of bingo numbers.
func TestBingoNumsCopy(t *testing.T) {
	testBingoNums := BingoNums{
		nums: []int{1},
	}

	testBingoNumsCopy := testBingoNums.Copy()
	testBingoNumsCopy.nums[0] = 2

	if testBingoNumsCopy.nums[0] == testBingoNums.nums[0] {
		t.Errorf("Detected uncopied bingo nums, "+
			"testBingoNums: %v, testBingoNumsCopy: %v\n",
			testBingoNums, testBingoNumsCopy)
	}
}

// It returns bingo from a bingo card and a list of input numbers.
func TestBingoCardCheckBingo(t *testing.T) {
	testBingoCard := BingoCard{
		rows: []BingoNums{
			BingoNums{
				nums: []int{1, 2, 3},
			},
		},
		cols: []BingoNums{
			BingoNums{
				nums: []int{4, 5, 6},
			},
		},
	}

	for _, num := range []int{1, 2, 1, 4, 7} {
		if testBingoCard.CheckBingo(num) {
			t.Errorf("Detected erroneous bingo with num %d, testBingoCard: %v\n",
				num, testBingoCard)
		}
	}

	if !testBingoCard.CheckBingo(3) {
		t.Errorf("Detected erroneous non-bingo, testBingoCard: %v\n",
			testBingoCard)
	}
}

// It copies a bingo card.
func TestBingoCardCopy(t *testing.T) {
	testBingoCard := BingoCard{
		rows: []BingoNums{
			BingoNums{
				nums: []int{1},
			},
		},
	}

	testBingoCardCopy := testBingoCard.Copy()
	testBingoCardCopy.rows[0].nums[0] = 2

	if testBingoCardCopy.rows[0].nums[0] == testBingoCard.rows[0].nums[0] {
		t.Errorf("Detected uncopied bingo card, "+
			"testBingoCard: %v, testBingoCardCopy: %v\n",
			testBingoCard, testBingoCardCopy)
	}
}

// It sums non-marked numbers on a bingo card.
func TestBingoCardSum(t *testing.T) {
	testBingoCard := BingoCard{
		rows: []BingoNums{
			BingoNums{
				nums: []int{-1, 2, 3},
			},
		},
	}

	got := testBingoCard.Sum()
	expected := 2 + 3

	if got != expected {
		t.Errorf("Got: %d, expected: %d\n", got, expected)
	}
}

// It converts row numbers to col numbers.
func TestReadNumsInput(t *testing.T) {
	got := ReadNumsInput("1 2 3")
	expected := []int{1, 2, 3}

	for i, _ := range got {
		if got[i] != expected[i] {
			t.Errorf("Got: %v, expected: %v\n", got, expected)
		}
	}
}

func TestRowsToCols(t *testing.T) {
	testRows := []BingoNums{
		BingoNums{nums: []int{1, 2, 3, 4}},
		BingoNums{nums: []int{5, 6, 7, 8}},
		BingoNums{nums: []int{9, 10, 11, 12}},
	}

	got := RowsToCols(testRows)
	expected := []BingoNums{
		BingoNums{nums: []int{1, 5, 9}},
		BingoNums{nums: []int{2, 6, 10}},
		BingoNums{nums: []int{3, 7, 11}},
		BingoNums{nums: []int{4, 8, 12}},
	}

	for i, nums := range got {
		for j, _ := range nums.nums {
			if got[i].nums[j] != expected[i].nums[j] {
				t.Errorf("Got: %v, expected: %v\n", got, expected)
			}
		}
	}
}
