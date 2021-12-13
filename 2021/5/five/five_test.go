package main

import "testing"

// It builds a grid.
func TestNewGrid(t *testing.T) {
  testGrid := Grid{
    points: []Point{
      Point{visits: 0, x: 0, y: 0,},
      Point{visits: 0, x: 1, y: 0,},
      Point{visits: 0, x: 2, y: 0,},
      Point{visits: 0, x: 0, y: 1,},
      Point{visits: 0, x: 1, y: 1,},
      Point{visits: 0, x: 2, y: 1,},
    },
  }

  testConstructedGrid := NewGrid(3, 2)

  for i, point := range testGrid.points {
    if point.visits != testConstructedGrid.points[i].visits &&
      point.x != testConstructedGrid.points[i].x &&
      point.y != testConstructedGrid.points[i].y {
      t.Errorf("Detected difference in grids, " +
        "testGrid: %v, testConstructedGrid: %v\n",
        testGrid, testConstructedGrid)
    }
  }
}

// It counts grid points by a visit count.
func TestGridCountPointsByVisits(t *testing.T) {
  testGrid := Grid{
    points: []Point{
      Point{visits: 0, x: 0, y: 0,},
      Point{visits: 1, x: 1, y: 0,},
      Point{visits: 1, x: 2, y: 0,},
      Point{visits: 1, x: 0, y: 1,},
      Point{visits: 2, x: 1, y: 1,},
      Point{visits: 3, x: 2, y: 1,},
    },
  }

  got := testGrid.countPointsByVisits(0)
  expected := 6
  if got != expected {
    t.Errorf("Got: %d, expected: %d\n", got, expected)
  }

  got = testGrid.countPointsByVisits(1)
  expected = 5
  if got != expected {
    t.Errorf("Got: %d, expected: %d\n", got, expected)
  }
}

// It finds points.
func TestGridFind(t *testing.T) {
  testGrid := Grid{
    points: []Point{
      Point{visits: 0, x: 0, y: 0,},
      Point{visits: 1, x: 1, y: 0,},
      Point{visits: 1, x: 2, y: 0,},
      Point{visits: 1, x: 0, y: 1,},
      Point{visits: 2, x: 1, y: 1,},
      Point{visits: 3, x: 2, y: 1,},
    },
  }

  point := testGrid.find(1, 1)
  if *point != testGrid.points[4] {
    t.Errorf("Got: %v, expected: %v\n", point, testGrid.points[4])
  }
}

// It traverses the grid.
func TestGridTraverse(t *testing.T) {
  testGrid := Grid{
    points: []Point{
      Point{visits: 0, x: 0, y: 0,},
      Point{visits: 0, x: 1, y: 0,},
      Point{visits: 0, x: 2, y: 0,},
      Point{visits: 0, x: 0, y: 1,},
      Point{visits: 0, x: 1, y: 1,},
      Point{visits: 0, x: 2, y: 1,},
    },
  }

  testGrid.traverse(Point{x: 0, y: 0,}, Point{x: 1, y: 0,})
  testGrid.traverse(Point{x: 1, y: 0,}, Point{x: 1, y: 1,})

  if testGrid.points[0].visits != 1 && 
    testGrid.points[1].visits != 2 &&
    testGrid.points[4].visits != 1 {
    t.Errorf("Traversal failed. Grid: %v\n", testGrid)
  }
}

// It visits points on a grid.
func TestGridVisit(t *testing.T) {
  testGrid := Grid{
    points: []Point{
      Point{visits: 0, x: 0, y: 0,},
      Point{visits: 1, x: 1, y: 0,},
    },
  }

  for _, point := range testGrid.points {
    testGrid.visit(point.x, point.y)
  }

  if testGrid.points[0].visits != 1 &&
    testGrid.points[1].visits != 2 {
    t.Errorf("Visits failed. Grid: %v\n", testGrid)
  }
}
