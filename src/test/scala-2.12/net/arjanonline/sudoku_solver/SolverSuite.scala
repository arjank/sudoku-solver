package net.arjanonline.sudoku_solver

import org.scalatest.FunSuite

class SolverSuite extends FunSuite {
  trait Board6x6 extends GameDef with StringBoardParser with Solver {
    override def blocksPerRow: Int = 2

    override val boardString: String =
      """6 . . 2 . .
        |1 . . 5 . .
        |. 1 6 . 3 2
        |2 . . 6 . .
        |4 . . 3 . .
        |. 5 2 . 6 4""".stripMargin
  }

  trait Board9x9 extends GameDef with StringBoardParser with Solver {
    override val boardString: String =
      """. . . . . 9 . . .
        |. 3 . 4 . . . 1 .
        |1 . . . . . 7 . .
        |. . . . 5 . 9 . 8
        |. . . . . 8 5 2 .
        |. . . . 3 . . 7 6
        |7 2 . 9 . . . 5 .
        |8 . . . . 1 . . .
        |5 4 . . 7 6 . . 2""".stripMargin
  }

  val solution6x6: String =
    """6 4 5 2 1 3
      |1 2 3 5 4 6
      |5 1 6 4 3 2
      |2 3 4 6 5 1
      |4 6 1 3 2 5
      |3 5 2 1 6 4""".stripMargin

  val solution9x9: String =
    """4 6 8 7 1 9 2 3 5
      |2 3 7 4 8 5 6 1 9
      |1 5 9 3 6 2 7 8 4
      |3 1 2 6 5 7 9 4 8
      |6 7 4 1 9 8 5 2 3
      |9 8 5 2 3 4 1 7 6
      |7 2 6 9 4 3 8 5 1
      |8 9 3 5 2 1 4 6 7
      |5 4 1 8 7 6 3 9 2""".stripMargin

  println(solution6x6)

  test("Solver should find correct solution for 6x6 sudoku") {
    new Board6x6 {
      assert(solution.get.toString === solution6x6)
    }
  }

  test("Solver should find only one solution") {
    new Board6x6 {
      assert(completeSolutions.length === 1)
    }
  }

  test("Solver should find correct solution for 9x9 sudoku") {
    new Board9x9 {
      assert(solution.get.toString === solution9x9)
    }
  }
}
