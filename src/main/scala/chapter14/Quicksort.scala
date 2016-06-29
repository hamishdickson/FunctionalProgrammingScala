package chapter14.quicksort

/**
  * Even though this has mutable state, it is still referentially transparent
  */
object Quicksort {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    var arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }

    def partition(n: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = n
      for (i <- n until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }

    def qs(n: Int, r: Int): Unit = if (n < r) {
      val pi = partition(n, r, n + (n - r) / 2)
      qs(n, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}
