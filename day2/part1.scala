import scala.io.Source
object App {
    final case class Position(
        horiz: Int,
        depth: Int
    )

    def main(args: Array[String]): Unit = {
        val inputs: List[(String, Int)] = Source.fromFile("input.txt").getLines.toList.map(i => {
            val ix = i.split(" ")
            (ix(0), ix(1).toInt)
        })

        val result = inputs.foldLeft(Position(0, 0))((pos: Position, row: (String, Int)) => {
            row._1 match {
                case "forward" => pos.copy(horiz = pos.horiz + row._2)
                case "down" => pos.copy(depth = pos.depth + row._2)
                case "up" => pos.copy(depth = pos.depth - row._2)
            }
        })

        println(result.horiz * result.depth)
    }
}