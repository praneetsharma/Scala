
/**
 * Tail recursive implementation of Factorial
 * 
 * Tail recursive implementations are optimized in scala. 
 */
object Factorial_TailRecursive {
  
  def factorial_tailRecursive (num: Int): Int = {
    def factorialLogic (curRes: Int, num: Int): Int = {
      if (num == 0) curRes
      else {
        factorialLogic(curRes * num, num - 1)
      }
    }
    factorialLogic(1, num)
  }
  
  def factorial_normal (num: Int): Int = {
    if (num == 0) 1
    else num * factorial_normal(num - 1)
  }
  
  def main (args: Array[String]): Unit = {
    println(factorial_tailRecursive (5))
    println(factorial_normal(5))
  }
  
}