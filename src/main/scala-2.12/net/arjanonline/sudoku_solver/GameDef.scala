package net.arjanonline.sudoku_solver

trait GameDef {

  def blocksPerColumn: Int = 3
  def blocksPerRow: Int = 3

  final def blockSize: Int = {
    blocksPerRow * blocksPerColumn
  }

  val availableCellValues: Range = 1 to blockSize
  val availableCoordinateValues: Range = 1 to blockSize

  type Row = Int
  type Column = Int
  type Block = Int

  case class Coordinate(row: Row, column: Column) {
    assert(availableCoordinateValues contains row)
    assert(availableCoordinateValues contains column)

    def block: Block = {
      val r = (row - 1) / blocksPerRow
      val c = (column - 1) / blocksPerColumn

      r * blocksPerRow + c + 1
    }
  }

  case class Cell(coordinate: Coordinate)(val value: Int) {
    assert(availableCellValues contains value)
  }

  case class Board(cells: List[Cell]) {
    def row(r: Row): List[Cell] = rows(r)
    def column(c: Column): List[Cell] = columns(c)
    def block(b: Block): List[Cell] = blocks(b)

    def isDefinedAt(coordinate: Coordinate): Boolean =
      cells exists (cell => cell.coordinate == coordinate)

    def add(cell: Cell): Board = {
      if (isDefinedAt(cell.coordinate))
        throw new IllegalStateException("Cannot redefine cell")
      else
        Board(cell::cells)
    }

    def isValid: Boolean = cells match {
      case c::_ =>
        val coordinate = c.coordinate
        isValidList(row(coordinate.row)) &&
          isValidList(column(coordinate.column)) &&
          isValidList(block(coordinate.block))
      case _ => true
    }

    def isValidList(cells: List[Cell]): Boolean =
      cells.map(cell => cell.value).toSet.size == cells.length

    lazy val rows: Map[Row, List[Cell]] =
      cells groupBy (cell => cell.coordinate.row)
    lazy val columns: Map[Column, List[Cell]] =
      cells groupBy (cell => cell.coordinate.column)
    lazy val blocks: Map[Int, List[Cell]] =
      cells groupBy (cell => cell.coordinate.block)
  }

  val board: Board
}
