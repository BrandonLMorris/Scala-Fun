import language.postfixOps

/** Implementations of Quicksort Algorithm */
object Quicksort {
  /* Non-functional Quicksort */
  def sort(xs: Array[Int]): Array[Int] = {
    // Helper to swap two elements in an array
    def swap(i: Int, j: Int) {
      val t = xs(i); xs(i) = xs(j); xs(j) = t
    }

    // Inner sorting function that does all the work
    def sort1(l: Int, r: Int): Array[Int] = {
      val pivot = xs((l + r) / 2)
      var i = l; var j = r
      while (i <= j) {
        while (xs(i) < pivot) i += 1
        while (xs(j) > pivot) j -= 1
        if (i <= j) {
          swap(i,j)
          i += 1
          j -= 1
        }
      }
      if (l < j) sort1(l, j)
      if (j < r) sort1(i, r)
      xs
    }

    // Call the inner function
    sort1(0, xs.length-1)
  }

  /** Functional (and more awesome) implementation */
  def funcSort(xs: Array[Int]): Array[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      Array.concat(
        sort(xs filter (pivot >)),
             xs filter (pivot ==),
        sort(xs filter (pivot <))
      )
    }
  }

  def main(args: Array[String]) = {
    var arr = Array[Int](5,4,3,2,1)
    println("Before sorting: "+ arr.mkString(", "))
    println("After sorting (imperative): " + sort(arr).mkString(", "))
    println("After sorting (functional): " + funcSort(arr).mkString(", "))
  }
}
