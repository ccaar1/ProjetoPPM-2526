package konane

import scala.collection.parallel.immutable.ParMap

// Required data types
type Coord2D = (Int, Int) // (row, column)
type Board = ParMap[Coord2D, Stone]

// Enum in Scala 3
enum Stone:
  case Black, White

  def opponent: Stone = this match
    case Stone.Black => Stone.White
    case Stone.White => Stone.Black

  def display: String = this match
    case Stone.Black => "B"
    case Stone.White => "W"
