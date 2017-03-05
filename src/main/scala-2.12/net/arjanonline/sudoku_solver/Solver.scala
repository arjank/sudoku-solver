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
      val boards = for (
        v <- availableCellValues
      ) yield board.add(Cell(coordinate)(v))

      boards filter (b => isValid(b))
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

  lazy val completeSolutions: Stream[Board] = {
    from(Stream((board, 0))) filter {case (b, _) => done(b)} map {case (b, _) => b}
  }

  lazy val solution: Option[Board] =
    completeSolutions.headOption
}
