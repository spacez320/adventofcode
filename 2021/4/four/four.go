/*

Advent of Code 2021, Day 4

Structs and utiilties to play bingo.

Usage: go run 4.go
*/

package main

type BingoNums struct {
	nums []int
}

type BingoCard struct {
	rows []BingoNums
	cols []BingoNums
}

// Determines if a bingo is present from a sequence of numbers on a card.
func (nums BingoNums) CheckBingo() bool {
	for _, num := range nums.nums {
		// If any number is non-negative, there is no bingo.
		if num >= 0 {
			return false
		}
	}

	return true
}

// Produces a copy of a bingo num.
func (nums BingoNums) Copy() (copyNums BingoNums) {
	copyNums.nums = make([]int, len(nums.nums))
	copy(copyNums.nums, nums.nums)

	return
}

// Deterimes if a bingo is present in a bingo card after calling a number.
func (card BingoCard) CheckBingo(calledNum int) bool {
	// Rows and columns to consider.
	candidateRowIndexes := []int{}
	candidateColIndexes := []int{}

	// Check rows for occurrences of the called number and record rows to check.
	for rowI, row := range card.rows {
		for i, num := range row.nums {
			if num == calledNum {
				// Mark out the element to indicate it's been selected.
				card.rows[rowI].nums[i] = -1
				candidateRowIndexes = append(candidateRowIndexes, rowI)
			}
		}
	}

	// Check cols for occurrences of the called number and record cols to check.
	for colI, col := range card.cols {
		for i, num := range col.nums {
			if num == calledNum {
				// Mark out the element to indicate it's been selected.
				card.cols[colI].nums[i] = -1
				candidateColIndexes = append(candidateColIndexes, colI)
			}
		}
	}

	// Look for bingo in rows.
	for _, candidateRowI := range candidateRowIndexes {
		if card.rows[candidateRowI].CheckBingo() {
			return true
		}
	}

	// Look for bingo in cols.
	for _, candidateColI := range candidateColIndexes {
		if card.cols[candidateColI].CheckBingo() {
			return true
		}
	}

	return false
}

// Produces a copy of a bingo card.
func (card BingoCard) Copy() (copyCard BingoCard) {
	copyCard = card
	copyCard.rows = make([]BingoNums, len(card.rows))
	copyCard.cols = make([]BingoNums, len(card.cols))

	// Copy rows.
	for i, row := range card.rows {
		copyCard.rows[i] = row.Copy()
	}

	// Copy cols.
	for i, col := range card.cols {
		copyCard.cols[i] = col.Copy()
	}

	return
}

// Sum numbers on a bingo card, ignoring marked out values.
func (card BingoCard) Sum() (sum int) {
	for _, row := range card.rows {
		for _, num := range row.nums {
			// Ignore -1 entries, indicating this entry is marked out.
			if num >= 0 {
				sum += num
			}
		}
	}

	return
}

// Converts a list of list of rows to a list of list of columns.
func RowsToCols(rows []BingoNums) (cols []BingoNums) {
	// Initialize cols.
	cols = make([]BingoNums, len(rows[0].nums))
	for i, _ := range cols {
		cols[i] = BingoNums{
			nums: make([]int, len(rows)),
		}
	}

	// Populate cols.
	for i, row := range rows {
		for j, num := range row.nums {
			cols[j].nums[i] = num
		}
	}

	return
}
