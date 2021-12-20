/*

Advent of Code 2021, Day 4

Structs and utiilties to play bingo.

Usage: go build && ./four

*/

package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	// Bingo cards in the game.
	var bingoCards []BingoCard
	// Input for the game.
	var bingoInput []int
	// Number that got the last bingo.
	var lastWinningBingoNum int
	// Card that got the last bingo.
	var lastWinningBingoCard BingoCard
	// Number that got bingo.
	var winningBingoNum int
	// Card that got bingo.
	var winningBingoCard BingoCard

	/*
		Read input.
	*/

	// Read the input file.
	file, _ := os.Open("4.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	// Parse game input.
	scanner.Scan()
	bingoInput = ReadNumsInput(scanner.Text())

	// Skip the first blank line.
	scanner.Scan()

	// Parse card input.
	nextBingoCard := BingoCard{}
	for scanner.Scan() {
		// Empty lines signal a new card.
		if scanner.Text() == "" {
			// Construct the columns of bingo numbers.
			nextBingoCard.cols = RowsToCols(nextBingoCard.rows)

			// Finalize the next bingo card.
			bingoCards = append(bingoCards, nextBingoCard)
			nextBingoCard = BingoCard{}

			continue
		}

		// Read the next rows of bingo numbers.
		nextBingoCard.rows = append(nextBingoCard.rows, BingoNums{
			nums: ReadNumsInput(scanner.Text()),
		})
	}

	/*
		Run a game of bingo and find a winning card.
	*/

	// Copy the bingo cards to preseve the original state.
	bingoCardsCopy := make([]BingoCard, len(bingoCards))
	for i, card := range bingoCards {
		bingoCardsCopy[i] = card.Copy()
	}

	// Find the first winning bingo card.
	bingo := false
	for _, num := range bingoInput {
		for _, bingoCard := range bingoCardsCopy {
			if bingoCard.CheckBingo(num) {
				bingo = true
				winningBingoNum = num
				winningBingoCard = bingoCard
				break
			}
		}

		// End the game if we have bingo.
		if bingo {
			break
		}
	}

	fmt.Println("Winning bingo:")
	winningBingoCardSum := winningBingoCard.Sum()
	fmt.Printf("Sum is: %d\n", winningBingoCardSum)
	fmt.Printf("Result is: %d\n", winningBingoCardSum*winningBingoNum)
	fmt.Println()

	/*
		Run games of bingo and find the last winning card.
	*/

	// Find the card with the longest bingo.
	var longestBingoLength int
	for _, bingoCard := range bingoCards {
		for i, num := range bingoInput {
			if bingoCard.CheckBingo(num) {
				// Detect if this is the yet longest bingo length.
				if i >= longestBingoLength {
					lastWinningBingoCard = bingoCard
					lastWinningBingoNum = num
					longestBingoLength = i
				}

				break
			}
		}
	}

	fmt.Println("Last winning bingo:")
	lastWinningBingoCardSum := lastWinningBingoCard.Sum()
	fmt.Printf("Sum is: %d\n", lastWinningBingoCardSum)
	fmt.Printf("Result is: %d\n", lastWinningBingoCardSum*lastWinningBingoNum)
}
