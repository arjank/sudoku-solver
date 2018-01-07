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
      (board.rows(c.row).map(_.value) contains v) ||
        (board.columns(c.column).map(_.value) contains v) ||
        (board.blocks(c.block).map(_.value) contains v)
    }

    def findHiddenSinglesArea(cellValues: Iterable[(Coordinate, Seq[Int])]): Iterable[(Coordinate, Seq[Int])] = {
      val uniqueNumbers = cellValues
        .flatMap(_._2)
        .groupBy(x => x)
        .mapValues(_.size)
        .filter(_._2 == 1)
        .keys
        .toList

      cellValues.map({
        case (n, s) if s.intersect(uniqueNumbers).nonEmpty => (n, s.intersect(uniqueNumbers))
        case (n, s) => (n, s)
      })
    }

    def findHiddenSingles(cellValues: Seq[(Coordinate, Seq[Int])]): Seq[(Coordinate, Seq[Int])] = {
      val columns = cellValues.groupBy(_._1.column).mapValues(findHiddenSinglesArea).values.flatten
      val rows = columns.groupBy(_._1.row).mapValues(findHiddenSinglesArea).values.flatten
      val blocks = rows.groupBy(_._1.block).mapValues(findHiddenSinglesArea).values.flatten

      blocks.toSeq
    }

    val coords = for {
      c <- 0 until totalCells
      if ! board.isDefinedAt(Coordinate(c))
    } yield Coordinate(c)

    val coordValues =
      coords
        .map(c => (c, availableCellValues.filterNot(valueFilter(c))))
        .sortBy(_._2.length)

    coordValues.headOption.map(_._2.length) match {
      case Some(n) if n <= 1 => coordValues
      case _ => findHiddenSingles(coordValues).sortBy(_._2.length)
    }
  }

  lazy val completeSolutions: Stream[Board] = {
    from(Stream(board)) filter (b => done(b))
  }

  lazy val isAmbiguous: Boolean = completeSolutions.take(2).length > 1

  lazy val solution: Option[Board] =
    completeSolutions.headOption
}
