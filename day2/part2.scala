import scala.io.Source
object App {
    final case class Position(
        horiz: Long,
        depth: Long,
        aim: Long
    )

    def main(args: Array[String]): Unit = {
        val inputs: List[(String, Long)] = Source.fromFile("input.txt").getLines.toList.map(i => {
            val ix = i.split(" ")
            (ix(0), ix(1).toLong)
        })

        val result = inputs.foldLeft(Position(0, 0, 0))((pos: Position, row: (String, Long)) => {
            row._1 match {
                case "forward" => pos.copy(
                    horiz = pos.horiz + row._2,
                    depth = pos.depth + (pos.aim * row._2)
                )
                case "down" => pos.copy(aim = pos.aim + row._2)
                case "up" => pos.copy(aim = pos.aim - row._2)
            }
        })

        println(result.horiz * result.depth)
    }
}