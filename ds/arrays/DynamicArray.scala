import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.io.StdIn.{readLine,readInt}

object Solution {
  def main(args: Array[String]) {
    val hm = HashMap[Int, ArrayBuffer[Int]]()
    val lines = io.Source.stdin.getLines
    val Array(buckets,_) = lines.next.split(" ").map(_.toInt)
    var lastAns = 0
  //  println("")
    for (ln <- lines) {
//      println(" LINE + " +ln)
      val Array(n,x,y) = ln.split(" ").map(_.toInt)

      val key = (x ^ lastAns) % buckets
      val things = hm.getOrElse(key, ArrayBuffer())

      if (n == 1) {
        things += y
        hm += key -> things
      } else if (n == 2) {
        lastAns = things(y % things.size)
        println(lastAns)
      }
    }
  }
}

/*

2 5
1 0 5
1 1 7
1 0 3
2 1 0
2 1 1
2 1 1
2 1 1


 */
