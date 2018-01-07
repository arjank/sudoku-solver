package net.arjanonline.sudoku_solver

import org.scalatest.FunSuite

class SolverSuite extends FunSuite {
  trait Board2x2 extends GameDef with StringBoardParser with Solver {
    override def blocksPerRow: Int = 2

    override def blocksPerColumn: Column = 2

    override val boardString: String =
      """1 . . 4
        |. 3 . .
        |. . 4 .
        |. . . 2""".stripMargin
  }

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

  trait Board9x9Hard extends GameDef with StringBoardParser with Solver {
    override val boardString: String =
      """. . . . . . . . 1
        |. . . . . . . 2 3
        |. . 4 . . 5 . . .
        |. . . 1 . . . . .
        |. . . . 3 . 6 . .
        |. . 7 . . . 5 8 .
        |. . . . 6 7 . . .
        |. 1 . . . 4 . . .
        |5 2 . . . . . . .""".stripMargin
  }

  // http://www.telegraph.co.uk/news/science/science-news/9360022/Worlds-hardest-sudoku-the-answer.html
  trait HardestSudoku extends GameDef with StringBoardParser with Solver {
    override val boardString: String =
      """8 . . . . . . . .
        |. . 3 6 . . . . .
        |. 7 . . 9 . 2 . .
        |. 5 . . . 7 . . .
        |. . . . 4 5 7 . .
        |. . . 1 . . . 3 .
        |. . 1 . . . . 6 8
        |. . 8 5 . . . 1 .
        |. 9 . . . . 4 . .""".stripMargin
  }

  val solution2x2: String =
    """1 2 3 4
      |4 3 2 1
      |2 1 4 3
      |3 4 1 2""".stripMargin

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

  val solution9x9Hard: String =
    """6 7 2 9 8 3 4 5 1
      |9 5 1 4 7 6 8 2 3
      |3 8 4 2 1 5 9 7 6
      |4 6 8 1 5 9 2 3 7
      |2 9 5 7 3 8 6 1 4
      |1 3 7 6 4 2 5 8 9
      |8 4 3 5 6 7 1 9 2
      |7 1 9 8 2 4 3 6 5
      |5 2 6 3 9 1 7 4 8""".stripMargin

  val solutionHardestSudoku: String =
    """8 1 2 7 5 3 6 4 9
      |9 4 3 6 8 2 1 7 5
      |6 7 5 4 9 1 2 8 3
      |1 5 4 2 3 7 8 9 6
      |3 6 9 8 4 5 7 2 1
      |2 8 7 1 6 9 5 3 4
      |5 2 1 9 7 4 3 6 8
      |4 3 8 5 2 6 9 1 7
      |7 9 6 3 1 8 4 5 2""".stripMargin

  test("Solver should find correct solution for 2x2 sudoku") {
    new Board2x2 {
      assert(solution.get.toString === solution2x2)
      assert(completeSolutions.length === 1)
    }
  }

  test("Solver should find correct solution for 6x6 sudoku") {
    new Board6x6 {
      assert(solution.get.toString === solution6x6)
    }
  }

  test("Solver should find only one solution for the 6x6 sudoku") {
    new Board6x6 {
      assert(completeSolutions.length === 1)
    }
  }

  test("Solver should find correct solution for 9x9 sudoku") {
    new Board9x9 {
      assert(solution.get.toString === solution9x9)
    }
  }

  test("Solver should find correct solution for hard 9x9 sudoku") {
    new Board9x9Hard {
      assert(solution.get.toString === solution9x9Hard)
    }
  }

  test("Solver should find correct solution for hardest sudoku") {
    new HardestSudoku {
      assert(solution.get.toString === solutionHardestSudoku)
    }
  }

  test("Solver should find only one solution for the 9x9 sudoku") {
    new Board9x9 {
      assert(completeSolutions.length === 1)
    }
  }

  test("Solver should find only one solution for hard 9x9 sudoku") {
    new Board9x9Hard {
      assert(completeSolutions.length === 1)
    }
  }

  test("Solver should find only one solution for hardest sudoku") {
    new HardestSudoku {
      assert(completeSolutions.length === 1)
    }
  }

  test("Solver should correctly identify allowed values") {
    new Board9x9 {
      assert(allowedCellValues(board).filter{ case (c, l) => c == Coordinate(0) }.head._2 === List(2, 4, 6))
    }
  }
}
