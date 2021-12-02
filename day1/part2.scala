import scala.io.Source
import scala.collection.immutable.Queue
object App {
    final case class Counts(
        window: Queue[Int],
        count: Int
    )

    def main(args: Array[String]) {
        val inputs: List[Int] = Source.fromFile("input.txt").getLines.toList.map(_.toInt)

        val (initialList, remaining) = inputs.splitAt(3)
        val initial = Counts(
            window = Queue(initialList:_*),
            0
        )

        val result = remaining.foldLeft(initial)((acc: Counts, num: Int) => {
            val (removedItem, newWindow) = acc.window.dequeue
            Counts(
                newWindow.appended(num),
                if (num > removedItem) acc.count + 1 else acc.count
            )
        })

        println(result.count)
    }
}