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

    def this(position: Int) = this(position % blockSize + 1, position / blockSize + 1)

    def block: Block = {
      val r = (row - 1) / blocksPerRow
      val c = (column - 1) / blocksPerColumn

      r * blocksPerRow + c + 1
    }
  }

  object Coordinate {
    def apply(position: Int) = new Coordinate(position)
  }

  case class Cell(coordinate: Coordinate)(val value: Int) {
    assert(availableCellValues contains value)
  }

  case class Board(cells: List[Cell]) {
    def isDefinedAt(coordinate: Coordinate): Boolean =
      cells exists (cell => cell.coordinate == coordinate)

    def add(cell: Cell): Board = {
      if (isDefinedAt(cell.coordinate))
        throw new IllegalStateException("Cannot redefine cell")
      else
        Board(cell::cells)
    }

    def add(extraCells: Iterable[Cell]): Board = {
      if (extraCells.exists(c => isDefinedAt(c.coordinate)))
        throw new IllegalStateException("Cannot redefine cell")
      else
        Board(cells ++ extraCells)
    }

    def isValid: Boolean = {
      rows.forall({case (_, cs: List[Cell]) => isValidList(cs)}) &&
        columns.forall({case (_, cs: List[Cell]) => isValidList(cs)}) &&
        blocks.forall({case (_, cs: List[Cell]) => isValidList(cs)})
    }

    def isValidList(cells: List[Cell]): Boolean =
      cells.map(cell => cell.value).toSet.size == cells.length

    lazy val rows: Map[Row, List[Cell]] =
      cells groupBy (cell => cell.coordinate.row) withDefaultValue List()
    lazy val columns: Map[Column, List[Cell]] =
      cells groupBy (cell => cell.coordinate.column) withDefaultValue List()
    lazy val blocks: Map[Int, List[Cell]] =
      cells groupBy (cell => cell.coordinate.block) withDefaultValue List()

    override lazy val toString: String = {
      def rowToString(cells: List[Cell]): String = {
        val empties =
          availableCoordinateValues
            .diff(cells.map(_.coordinate.column))
            .map((_, "."))

        cells
          .map(c => (c.coordinate.column, c.value))
          .++(empties)
          .sortBy(_._1)
          .map(_._2)
          .mkString(" ")
      }

      val rs = for {
        r <- availableCoordinateValues
      } yield rows(r)

      rs.map(rowToString).mkString("\n")
    }
  }

  val board: Board
}
