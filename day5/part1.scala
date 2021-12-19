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

    def isVerticalOrHorizontal(l: Line): Boolean = 
        isVertical(l) || isHorizontal(l)

    def fillLine(a: Array[Array[Int]])(l: Line): Unit = {
        val xInc = if (l.a.x < l.b.x) 1 else -1
        val yInc = if (l.a.y < l.b.y) 1 else -1

        for (y <- l.a.y to l.b.y by yInc) {
            for (x <- l.a.x to l.b.x by xInc) {
                a(y)(x) += 1
            }
        }
    }

    def main(args: Array[String]): Unit = {
        val input: List[String] = Source.fromFile("input.txt").getLines.toList

        val vertices: List[Line] = input
            .map(
                _.split(" -> ").map(
                    _.split(",").map(_.toInt)
                ).map(p => Point(p(0), p(1)))
                
            ).map(p => Line(p(0), p(1)))

        // vertices.map(isVerticalOrHorizontal).foreach(println(_))

        val considered = vertices.filter(isVerticalOrHorizontal)

        val seafloor = Array.fill(1000, 1000)(0)

        considered.foreach(fillLine(seafloor)(_))

        println(seafloor.flatten.count(_ >= 2))

        // seafloor.map(_.toList).foreach(println)
        // println(seafloor.toList.map(_.toList))
    }
}