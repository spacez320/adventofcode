package main

import (
  "bufio"
  "fmt"
  "math"
  "os"
  "regexp"
  "strconv"
)

func main() {
  // Grid of points.
  var grid Grid
  // Height of the grid.
  var gridHeight int
  // Width of the grid.
  var gridWidth int
  // Point traversal input.
  var traversals [][]Point

  /*
    Read input.
  */

  // Read the input file.
  file, _ := os.Open("five.txt")
  defer file.Close()
  scanner := bufio.NewScanner(file)
  scanner.Split(bufio.ScanLines)

  // Parse instructions.
  r := regexp.MustCompile(`\d+`)
  for scanner.Scan() {
    nextPointValues := []int{}
    nextPoints := []Point{}
    for _, match := range r.FindAllString(scanner.Text(), -1) {
      nextPointValue, _ := strconv.Atoi(match)
      nextPointValues = append(nextPointValues, nextPointValue)
    }

    nextPoints = []Point{
      Point{x: nextPointValues[0], y: nextPointValues[1]},
      Point{x: nextPointValues[2], y: nextPointValues[3]},
    }

    traversals = append(traversals, nextPoints)
  }

  /*
    Initialize and traverse the grid.
  */

  // Determine grid dimensions.
  for _, points := range traversals {
    for _, point := range points {
      if point.x > gridWidth {
        gridWidth = point.x
      }
      if point.y > gridHeight {
        gridHeight = point.y
      }
    }
  }

  grid = NewGrid(gridWidth + 1, gridHeight + 1)

  // Traverse the grid.
  for _, traversal := range traversals {
    fmt.Printf("Traversing: %v ...\n", traversal)

    // Focus only on straight line or 45deg traversals.
    if ! (traversal[0].x == traversal[1].x ||
      traversal[0].y == traversal[1].y ||
      int(math.Abs(float64(traversal[0].x - traversal[1].x))) ==
      int(math.Abs(float64(traversal[0].y - traversal[1].y)))) {
      continue
    }

    grid.traverse(traversal[0], traversal[1])
  }

  grid.pprint()

  // Find the grid visitation summation.
  fmt.Printf("Grid points visited at least twice: %d\n",
    grid.countPointsByVisits(2))
}
