package net.arjanonline.sudoku_solver

trait Solver extends GameDef {

  lazy val totalCells: Int = blockSize * blockSize

  def done(b: Board): Boolean =
    b.cells.length == totalCells

  def isValid(b: Board): Boolean = b.cells match {
    case c::_ =>
      b.isValidList(b.rows(c.coordinate.row)) &&
        b.isValidList(b.columns(c.coordinate.column)) &&
        b.isValidList(b.blocks(c.coordinate.block))
    case _ => true
  }

  def possibleMoves(board: Board, coordinate: Coordinate): Seq[Board] = {
    if (board.isDefinedAt(coordinate)) Seq(board)
    else {
      val allowedValues = allowedCellValues(board)

      if (allowedValues.exists {case (_, l) => l.isEmpty }) Seq()
      else
        for {
          v <- allowedValues(coordinate)
        } yield board.add(Cell(coordinate)(v))
    }
  }

  def from(initial: Stream[(Board, Int)]): Stream[(Board, Int)] = {
    if (initial.isEmpty || initial.head._2 >= totalCells) Stream.Empty
    else {
      val n = for {
        (b, p) <- initial
        if p < totalCells
        board <- possibleMoves(b, Coordinate(p))
      } yield (board, p + 1)

      initial #::: from(n)
    }
  }

  def allowedCellValues(board: Board): Map[Coordinate, List[Int]] = {
    def valueFilter(c: Coordinate)(v: Int): Boolean = {
      (board.rows.withDefaultValue(List())(c.row).map(_.value) contains v) ||
        (board.columns.withDefaultValue(List())(c.column).map(_.value) contains v) ||
        (board.blocks.withDefaultValue(List())(c.block).map(_.value) contains v)
    }

    val coords = for {
      c <- 0 until totalCells
      if ! board.isDefinedAt(Coordinate(c))
    } yield Coordinate(c)

    coords.map(c => (c, availableCellValues.toList.filterNot(valueFilter(c)))).toMap
  }

  lazy val completeSolutions: Stream[Board] = {
    from(Stream((board, 0))) filter {case (b, _) => done(b)} map {case (b, _) => b}
  }

  lazy val solution: Option[Board] =
    completeSolutions.headOption
}
