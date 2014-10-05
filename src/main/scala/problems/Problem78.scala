package problems

/**
 * Theory from http://mathworld.wolfram.com/PartitionFunctionP.html
 *
 * Solved: 05.10.2014
 */
object Problem78 {

  def main(args: Array[String]): Unit = {
    def search(predicate: Int => Boolean, n: Int = 1): Int = if (predicate(n)) n else search(predicate, n + 1)
    println(search(dp(_) % 1000000 == 0))
  }

  def p(n: Int): BigInt =
    if (n == 0) 1
    else if (n < 0) 0
    else (for {
        k   <- 1 to math.ceil((1 + math.sqrt(1 + 24 * n)) / 6).toInt
        p1   = k * (3 * k - 1) / 2
        p2   = k * (3 * k + 1) / 2
        sign = if (k % 2 == 0) -1 else 1
      } yield sign * (dp(n - p1) + dp(n - p2))
    ).sum

  val dp = dynamic(p)

  def dynamic[A, B](f: A => B): A => B = {
    val m = collection.mutable.Map[A, B]()
    x: A => m get x match {
      case Some(r) => r
      case None    => m(x) = f(x); m(x)
    }
  }

}