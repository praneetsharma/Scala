

object Quicksort {
  
  // Functional programming paradigm used to code quicksort
  def quicksortFunctional(arr: Array[Int]): Array[Int] = {
    if (arr.length < 1) arr
    else {
      val pivot = arr(arr.length/2)
      Array.concat(
          quicksortFunctional (arr filter (pivot >)),
          arr filter (pivot ==),
          quicksortFunctional (arr filter (pivot <))
      )
    }
  }
  
  // Imperative programming paradigm used to code quicksort
  def quicksortImperative(arr: Array[Int]) {
    def swap(i: Int, j: Int): Unit = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }
    def quicksortAux(l: Int, r: Int) {
      val pivot: Int = arr((l + r) / 2)
      var i: Int = l; var j: Int = r;
      while (i <= j) {
        while (arr(i) < pivot) i = i + 1
        while (arr(j) > pivot) j = j - 1
        if (i <= j) {
          swap(i, j)
          i = i + 1
          j = j - 1
        }
        if (j > l) quicksortAux(l, j)
        if (i < r) quicksortAux(i, r)
      }
    }
    quicksortAux(0, arr.length - 1)
  }
  
  def main (args: Array[String]): Unit = {
    var arr = Array(5,2,7,1,9,3)
    var arr2 = Array(5,2,7,1,9,3)
    
    println(arr.mkString(" "))
    
    println(quicksortFunctional(arr).mkString(" "))
    
    quicksortImperative(arr)
    println(arr.mkString(" "))    
  }
  
}