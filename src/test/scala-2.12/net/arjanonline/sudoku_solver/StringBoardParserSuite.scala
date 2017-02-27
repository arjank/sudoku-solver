package net.arjanonline.sudoku_solver

import org.scalatest.FunSuite

class StringBoardParserSuite extends FunSuite {

  trait SimpleBoard extends StringBoardParser {

    val boardString: String = ""
  }

  trait Board66 extends StringBoardParser {
    override def blocksPerRow: Int = 2

    override val boardString: String =
      """1 2 3 4 5 6
        |2 3 4 5 6 1
        |3 4 5 6 1 2
        |4 5 6 1 2 3
        |5 6 1 2 3 4
        |6 1 2 3 4 5""".stripMargin
  }

  test("Same row + column is same cell") {
    new SimpleBoard {
      val c1 = Cell(Coordinate(1,1))(3)
      val c2 = Cell(Coordinate(1,1))(2)

      private val combined = Set(c1, c2)

      assert(combined.size === 1)
      assert(combined.head.value === c1.value)
    }
  }

  test("Cell calculates block correctly") {
    new SimpleBoard {
      override def blocksPerRow: Int = 2

      val c1 = Coordinate(1,1)
      val c2 = Coordinate(1,6)
      val c3 = Coordinate(5,1)
      val c4 = Coordinate(6,6)

      assert(c1.block === 1)
      assert(c2.block === 2)
      assert(c3.block === 5)
      assert(c4.block === 6)
    }
  }

  test("foo") {
    new Board66 {
      assert(board.cells.size === 36)
    }
  }
}
