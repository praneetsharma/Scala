

class Point (var x: Int, var y: Int) extends Similarity {
  
  // 2 points are similar if their x-coordinates match
  def isSimilar(obj: Any): Boolean = {
    obj.isInstanceOf[Point] &&
    obj.asInstanceOf[Point].x == x
  }
  
  def move (dx: Int, dy: Int): Unit = {
    x = x + dx
    y = y + dy
  }
  
  override def toString: String = {
    "(" + x + "," + y + ")"
  }
}

object PointTest {
  def main(args: Array[String]): Unit = {
    val pt = new Point(4,5)
    println(pt)
    pt.move(1, 2)
    println(pt)
    
    val pt2 = new Point(5,7);
    println(pt2.isSimilar(pt))
    
  }
}