object LongestCommonSubsequence {

  def LCS[A](xs: Array[A], ys: Array[A]): Int = {
    var L = Array.ofDim[Int](2, Math.max(xs.length,ys.length)+1)

    for ( (x,i) <- xs.zipWithIndex ) {
      for ( (y,j) <- ys.zipWithIndex ) {
        if (x == y)
          L(1)(j+1) = 1 + L(0)(j)
        else
          L(1)(j+1) = Math.max(L(0)(j+1), L(1)(j))
      }
      L(0) = L(1)
    }
    L(1)(ys.length)
  }

  def main(args: Array[String]) {
    val _  = readLine
    val xs = readLine.split(" ").map(_.toInt)
    val ys = readLine.split(" ").map(_.toInt)
    println(LCS(xs,ys))
  }

}
