package utils

object Matrix {

  def prettyPrint(matrix: Array[Array[Double]]) {
    for (i <- 0 to matrix.length - 1) {

      for (j <- 0 to matrix(0).length - 1) {
        print(("%.2f" format matrix(i)(j)) + ", ")
      }
      println()
    }
  }
  
  def prettyPrint(matrix: Array[Array[Int]])
  {
    val str = matrix.map(_.mkString(", ")).mkString("\n")
    println(str)
  }
  
  def transpose(matrix: Array[Array[Int]]):Array[Array[Int]] =
  {
    val n = matrix.length
    val mtx = Array.ofDim[Int](n, n)
    for(i <- 0 until n; j <- 0 until n)
      mtx(j)(i) = matrix(i)(j)
      
    mtx
  }

  /**
   * defines a multiplication of two integer matrices.
   *  We could write a general purpose method, but we don't need it for now.
   */
  def intMultiply(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Array[Array[Int]] =
    {
      if (matrix1(0).length != matrix2.length) {
        throw new RuntimeException("matrices dimension mismatch: [" + matrix1.length + "," + matrix1(0).length + "] and [" + matrix2.length + "," + matrix2(0).length + "]")
      }
      
      var result = Array.ofDim[Int](matrix1.length, matrix2(0).length)
      
      for(i <- 0 until matrix1.length;
      j <- 0 until matrix2(0).length)
        {
    	  result(i)(j) = 0
    	  // k is the middle variable
    	  // m(i,j) = m1(i, 0)*m2(0, j) + m1(i, 1)*m2(1, j) + ...
          for(k <- 0 until matrix1(0).length)
          {
            result(i)(j) += matrix1(i)(k) * matrix2(k)(j)
          }    	   
        }
      
      result
    }

}