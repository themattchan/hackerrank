object CoinChange {
  def countChange_dp(v: Int, coins: List[Int]): BigInt = {
    var t = Array.fill(v+1)(BigInt(0))
    t(0) = 1
    for { c <- coins
          a <- c to v } {
      t(a) += t(a-c)
    }
    t(v)
  }

  def main(args: Array[String]) {
    val v = readLine.split(" ").map(_.toInt).head
    val cs = readLine.split(" ").map(_.toInt).toList
    println(countChange_dp(v,cs))
  }
}
