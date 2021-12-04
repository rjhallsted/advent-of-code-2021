import scala.io.Source
object App {
    def isMostCommonBitAtPositionAOne(width: Int, pos: Int)(inputs: List[String]): Boolean = {
        val count = inputs.foldLeft(0)((counts: Int, row: String) => {
            val valAt = row.split("")(pos)

            if (valAt == "1") counts + 1 else counts - 1
        })
        if (count >= 0) true else false
    }

    def keepOnlyValuesWithBitAtPos(one: Boolean, pos: Int)(values: List[String]): List[String] = {
        val bitToKeep = if (one) "1" else "0"
        values.filter(_.split("")(pos) == bitToKeep)
    }

    def findO2rating(inputs: List[String], width: Int, currentPos: Int = 0): String = {
        val mc = isMostCommonBitAtPositionAOne(width, currentPos)(inputs)
        val newVals = keepOnlyValuesWithBitAtPos(mc, currentPos)(inputs)
        if (newVals.size == 1) newVals.head else findO2rating(newVals, width, currentPos + 1)
    }

    def findCO2rating(inputs: List[String], width: Int, currentPos: Int = 0): String = {
        val mc = isMostCommonBitAtPositionAOne(width, currentPos)(inputs)
        val newVals = keepOnlyValuesWithBitAtPos(!mc, currentPos)(inputs)
        if (newVals.size == 1) newVals.head else findCO2rating(newVals, width, currentPos + 1)
    }

    def main(args: Array[String]): Unit = {
        val inputs: List[String] = Source.fromFile("input.txt").getLines.toList

        val o2 = findO2rating(inputs, inputs.head.size)
        val co2 = findCO2rating(inputs, inputs.head.size)

        println(Integer.parseInt(o2, 2) * Integer.parseInt(co2, 2))
    }
}