

object CurryTest {
  def main(args: Array[String]): Unit = {
    val nums = List(1, 2, 3, 4, 5, 6, 7, 8)
    
    println(filter(nums, modN(2)))
    println(filter(nums, modN(3)))
  }
  
  def filter(list: List[Int], f: Int => Boolean): List[Int] = {
    if (list.isEmpty) list
    else if (f(list.head)) list.head :: filter(list.tail, f)
    else filter(list.tail, f)
  }
  
  def modN(n: Int)(x: Int) = ((x % n) == 0)
}