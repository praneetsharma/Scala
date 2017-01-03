

object NestedFunctionsExample {
  def main(args: Array[String]): Unit = {
    println(filter(List(1, 9, 2, 8, 3, 7, 4), 5))
  }
  
  // a recursive nested function "process" inside
  // function "filter"
  def filter(list: List[Int], threshold: Int): List[Int] = {
    def process(l: List[Int]): List[Int] = {
      if (l.isEmpty) l
      else if (l.head < threshold) {
        l.head :: process(l.tail) // l.tail returns all elements of list except first
      } else {
        process(l.tail) // l.tail returns all elements of list except first
      }
    }
    process(list) // calling the recursive function
  }
}