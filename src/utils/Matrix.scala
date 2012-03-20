package util

object Matrix {

  def prettyPrint(matrix:Array[Array[Double]])
  {
    for (i <- 0 to matrix.length - 1) {
      
      for (j <- 0 to matrix(0).length - 1) {
        print(("%.2f" format matrix(i)(j)) + ", ")
      }
      println()
    }
  }
}