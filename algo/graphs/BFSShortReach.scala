object BFSShortReach {
  type Node = Int
  type GraphM = Map[Node, List[Node]]

  class Graph(numNodes: Int, graph: GraphM, start: Node) {
    import scala.collection.mutable.Queue

    val edgeLength = 6
    val INFINITY = -1

    def bfs(): Array[Int] = {
      val bfsRet: Array[Option[Int]] = Array.fill(numNodes+1) {None}

      var Q = Queue[Int]()

      bfsRet(start) = Some(0)
      Q.enqueue(start)

      while (!Q.isEmpty) {
        val cur = Q.dequeue
        for (adj <- graph(cur)) {
          if (bfsRet(adj).isEmpty) {
            bfsRet(adj) = bfsRet(cur).map(_ + edgeLength)
            Q.enqueue(adj)
          }
        }
      }

      bfsRet.tail.map(_.getOrElse(INFINITY)).filterNot(_ == 0)
    }

  }

  // wtf is wrong with hackerrank's readInt
  def readOne() = readLine.trim.toInt

  def readTwo() = {
    val Array(x,y) = readLine.split(" ").map(_.toInt)
    (x,y)
  }

  def readGraph(): Graph = {
    var graph: GraphM = Map()
    val (n,m) = readTwo()

    for (i <- 1 to n)
      graph = graph updated (i,Nil)

    for (i <- 1 to m) {
      val (x,y) = readTwo()
      val from = graph getOrElse (x, Nil)
      val to = graph getOrElse (y, Nil)
      graph = graph updated (x, y::from)
      graph = graph updated (y, x::to)
    }

    val start = readOne()
    new Graph(n, graph, start)
  }

  def main(args: Array[String]) = {
    val T = readOne()
    for (_ <- 1 to T) {
      val g = readGraph()
      println(g.bfs().mkString(" "))
    }
  }
}
