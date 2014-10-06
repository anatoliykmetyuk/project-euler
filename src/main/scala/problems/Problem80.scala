package problems

import java.math.MathContext

/**
 * Solved: 06.10.2014
 */
object Problem80 {

  def main(args: Array[String]): Unit = {
    def sqrt(x: BigDecimal, guess: BigDecimal): BigDecimal = {
      val d = x / guess
      if ((d - guess).abs <= BigDecimal(1, d.mc.getPrecision - 1, d.mc)) guess
      else sqrt(x, (guess + d) / 2)
    }

    val result = (for {
      i <- 1 to 100 if !math.sqrt(i).isWhole
      j = BigDecimal(i, new MathContext(102))
      n <- sqrt(j, j / 2).toString.filter(_ != '.').take(100).map(_ - '0')
    } yield n).sum

    println(result)
  }

}
