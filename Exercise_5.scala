

object Exercise_5 {
  
  // Exercise 5.2.1
  def sum_TailRecursive (f: Int => Int)(a: Int, b: Int): Int = {
    def sum_TailRecursiveAux (a: Int, curOut: Int): Int = {
      if (a <= b) sum_TailRecursiveAux (a + 1, curOut + a) else curOut
    }
    sum_TailRecursiveAux (a, 0)
  }
  
  // Exercise 5.2.2
  def product (f: Int => Int, a: Int, b: Int): Int = {
    if (a <= b) f(a) * product(f, a+1, b)
    else 1
  }
  
  def productCurry (f: Int => Int)(a: Int, b: Int): Int = {
    if (a <= b) f(a) * productCurry(f)(a+1, b)
    else 1
  }
  
  // Exercise 5.2.3
  def factorial_UsingProduct (f: Int => Int)(productFunc: (Int => Int, Int, Int) => Int)(a: Int): Int = {
    productFunc(f, 1, a)
  }
  
  // Exercise 5.2.4
  def operationInRange (default: Int)(operator: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int): Int = {
    if (a <= b) operator(f(a), operationInRange(default)(operator)(f)(a+1, b))
    else default
  }
  
  
  // Exercise 5.3.1
  // y*y*y = x
  // y = x/(y*y)
  // Hence, fixedPoint formula becomes y => x/(y*y)
  def cubeRoot (a: Double): Double = {
    val tolerance = 0.001
    def isCorrectGuess (guess: Double): Boolean = {
      if ((guess*guess*guess - a) < tolerance) true else false
    }
    def averageDamp (f: Double => Double)(y: Double): Double = {
      (y + f(y)) / 2
    }
    def fixedPoint (f: Double => Double)(firstGuess: Double): Double = {
      def iterate(guess: Double): Double = {
        if (isCorrectGuess(guess)) guess
        else iterate(averageDamp(f)(guess))
      }
      iterate(firstGuess)
    }
    
    fixedPoint (averageDamp(y => a/(y*y)))(1.0)
  }
  
  
  def main (args: Array[String]): Unit = {
    def f(a: Int): Int = a
    
    println(sum_TailRecursive(f)(1, 5))
    
    println(product(f,3,5))
    println(productCurry(f)(3,5))
    
    println(factorial_UsingProduct(f)(product)(5))
    
    def mult(a: Int, b: Int): Int = a * b
    def add(a: Int, b: Int): Int = a + b
    
    println(operationInRange(1)(mult)(f)(3,5))
    println(operationInRange(0)(add)(f)(1,5))
    
    
    println(cubeRoot(2))
  }
  
}