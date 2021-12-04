import scala.io.Source
object App {

    def main(args: Array[String]): Unit = {
        val inputs: List[String] = Source.fromFile("input.txt").getLines.toList

        val counts = List.fill(inputs(0).size)(0)
        println(counts)

        val result = inputs.foldLeft(counts)((counts: List[Int], row: String) => {
            val newCounts = row.split("").map(_ match {
                case "1" => 1
                case "0" => -1
                case _ => 0
            }).zip(counts).map(x => x._2 + x._1).toList
            println(newCounts.toList)
            newCounts
        })

        val gamma = result.map(_ match {
            case x if x >= 0 => "1"
            case _ => "0"
        }).mkString

        val epsilon = result.map(_ match {
            case x if x >= 0 => "0"
            case _ => "1"
        }).mkString

        println(gamma)
        println(epsilon)

        println(Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2))

    }
}