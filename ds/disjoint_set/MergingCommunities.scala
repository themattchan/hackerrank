class UnionFind {
  private var sets: Option[Array[Int]] = None
  private var components: Option[Int] = None

  def this(maxSize: Int) = {
    this.sets = Some((0 to (maxSize-1)).map(x => (x,1)).toArray)
    this.components = Some(maxSize)
  }

  def find(e : Int) = {
    var root = e
    while (root != sets(root)._1) {
      root = sets(root)._1
    }
    while (e != root) {
      var newe = sets(e)
      sets(e)._1 = root
  }

}
