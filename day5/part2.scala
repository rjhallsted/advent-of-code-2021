import scala.io.Source
object App {
    final case class Point(
        x: Int,
        y: Int
    )

    final case class Line(
        a: Point,
        b: Point
    )

    def isVertical(l: Line): Boolean = l.a.x == l.b.x

    def isHorizontal(l: Line): Boolean = l.a.y == l.b.y

    def fillLine(a: Array[Array[Int]])(l: Line): Unit = {
        val xInc = if (l.a.x < l.b.x) 1 else -1
        val yInc = if (l.a.y < l.b.y) 1 else -1

        val points = l match {
            case l if isVertical(l) => ((l.a.y to l.b.y by yInc) zip Stream.continually(l.a.x)).toVector
            case l if isHorizontal(l) => (Stream.continually(l.a.y) zip (l.a.x to l.b.x by xInc)).toVector
            case _ => (l.a.y to l.b.y by yInc) zip (l.a.x to l.b.x by xInc)
        }

        points.foreach(p => {
            a(p._1)(p._2) += 1
        })
    }

    def main(args: Array[String]): Unit = {
        val input: List[String] = Source.fromFile("input.txt").getLines.toList

        val lines: List[Line] = input
            .map(
                _.split(" -> ").map(
                    _.split(",").map(_.toInt)
                ).map(p => Point(p(0), p(1)))
                
            ).map(p => Line(p(0), p(1)))

        val seafloor = Array.fill(1000, 1000)(0)

        lines.foreach(fillLine(seafloor)(_))

        println(seafloor.flatten.count(_ >= 2))
    }
}