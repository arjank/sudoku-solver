package net.arjanonline.sudoku_solver

trait Solver extends GameDef {

  lazy val totalCells: Int = blockSize * blockSize

  def done(b: Board): Boolean =
    b.cells.length == totalCells

  def from(initial: Stream[Board]): Stream[Board] = {
    if (initial.isEmpty) Stream.Empty
    else {
      val boards = for {
        board <- initial
        cellValues = allowedCellValues(board)
        if cellValues.nonEmpty
        (coordinate, values) = cellValues.head
        value <- values
      } yield board.add(Cell(coordinate)(value))

      initial #::: from(boards)
    }
  }

  def allowedCellValues(board: Board): Seq[(Coordinate, Seq[Int])] = {
    def valueFilter(c: Coordinate)(v: Int): Boolean = {
      (board.rows.withDefaultValue(List())(c.row).map(_.value) contains v) ||
        (board.columns.withDefaultValue(List())(c.column).map(_.value) contains v) ||
        (board.blocks.withDefaultValue(List())(c.block).map(_.value) contains v)
    }

    val coords = for {
      c <- 0 until totalCells
      if ! board.isDefinedAt(Coordinate(c))
    } yield Coordinate(c)

    coords.map(c => (c, availableCellValues.filterNot(valueFilter(c)))).sortBy(_._2.length)
  }

  lazy val completeSolutions: Stream[Board] = {
    from(Stream(board)) filter (b => done(b))
  }

  lazy val isAmbiguous: Boolean = completeSolutions.take(2).length > 1

  lazy val solution: Option[Board] =
    completeSolutions.headOption
}
