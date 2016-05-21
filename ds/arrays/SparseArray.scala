object Solution {
  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines
    val N = lines.next.toInt
    val map = (1 to N).foldLeft(Map[String,Int]()) { (map, _) =>
        val s = lines.next
        val count = map.getOrElse(s,0) +1
        map + (s -> count)
      }

    val Q = lines.next.toInt
    for (_ <- 1 to Q) {
      val s = lines.next
      println(map.getOrElse(s,0))
    }
  }
}
