package net.arjanonline.sudoku_solver

trait StringBoardParser extends GameDef {

  val boardString: String

  def boardFunction(vector: Vector[Vector[String]]): Board = {
    val cells = for {
      r <- 1 to blockSize
      c <- 1 to blockSize
      if vector(r - 1)(c - 1) != "."
    } yield Cell(Coordinate(r, c))(vector(r - 1)(c - 1).toInt)

    Board(cells.toList)
  }

  private lazy val vector: Vector[Vector[String]] =
    Vector(boardString.split("\n").map(str => Vector(str.split(" "): _*)): _*)

  lazy val board: Board = boardFunction(vector)
}
