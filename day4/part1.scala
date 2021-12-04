import scala.io.Source
object App {
    case class State(
        picksLeft: List[Int],
        boards: List[Board]
    )

    case class Board(
        v: List[List[Space]]
    )

    case class Space(
        value: Int,
        marked: Boolean
    )

    def doesBoardHaveBingo(b: Board): Boolean = {
        def checkRows: Boolean =
            b.v.filter(_.filter(_.marked).size == 5).nonEmpty

        def checkColumns: Boolean = {
            def checkAt(pos: Int): Boolean = {
                b.v.map(_(pos)).map(_.marked).filter(identity).size == 5
            }

            List.range(0, 5).map(checkAt(_)).find(identity).nonEmpty
        }

        def checkDiagonals: Boolean = {
            val positions = List.range(0, 5)

            val topLeftToBottomRight = positions.map(p => b.v(p)(p)).filter(_.marked).size == 5
            val topRightToBottomLeft = positions.map(p => b.v(4 - p)(p)).filter(_.marked).size == 5

            topLeftToBottomRight || topRightToBottomLeft
        }

        checkRows || checkColumns || checkDiagonals
    }

    def findWinningScore(state: State): Int = {
        val pick = state.picksLeft.head
        val updatedBoards = state.boards.map(markSpace(_, pick))

        val winner = updatedBoards.find(doesBoardHaveBingo(_))

        winner
            .map(b => boardPoints(b, pick))
            .getOrElse(
                findWinningScore(State(
                    picksLeft = state.picksLeft.tail,
                    boards = updatedBoards
                ))
            )
    }

    def replaceWith[V](list: List[V], shouldReplace: V => Boolean, newVal: V): List[V] =
        list match {
            case h :: t if shouldReplace(h) => newVal :: replaceWith(t, shouldReplace, newVal)
            case h :: t => h :: replaceWith(t, shouldReplace, newVal)
            case Nil => Nil
        }

    def markSpace(board: Board, num: Int): Board = {
        Board(
            board.v.map(l => replaceWith(l, (s: Space) => s.value == num, Space(num, true)))
        )
    }

    def createBoards(input: List[List[String]]): List[Board] = {
        input.map(rows => Board(
            rows.map(_.split(" ").filter(_.trim.nonEmpty).map(v => Space(v.toInt, false)).toList)
        ))
    }

    def boardPoints(board: Board, lastPick: Int): Int = {
       board.v.flatten.filter(!_.marked).map(_.value).sum * lastPick
    }
    

    def main(args: Array[String]): Unit = {
        val input: String = Source.fromFile("input.txt").iter.mkString

        val parts: List[String] = input.split("\n\n").toList

        val picks = parts.head.split(",").map(_.toInt).toList
        val boards = createBoards(parts.tail.map(_.split("\n").toList))

        val result = findWinningScore(State(picks, boards))

        println(result)
    }
}