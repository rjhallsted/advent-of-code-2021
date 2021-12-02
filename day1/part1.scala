import scala.io.Source
object App {
    final case class Counts(
        lastItem: Option[Int],
        count: Int
    )

    def main(args: Array[String]) {
        val inputs: List[Int] = Source.fromFile("input1.txt").getLines.toList.map(_.toInt)

        val result = inputs.foldLeft(Counts(None, 0))((acc: Counts, num: Int) => {
            Counts(
                Some(num),
                acc.lastItem.filter(_ < num).map(_ => acc.count + 1).getOrElse(acc.count)
            )
        })

        println(result.count)
    }
}