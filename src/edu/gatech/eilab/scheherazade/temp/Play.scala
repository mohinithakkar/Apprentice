//package edu.gatech.eilab.scheherazade.temp
//
///** playing with the Breeze library
// * 
// */
//import breeze.linalg._
//import breeze.serialization._
//import breeze.io._
//import breeze.io.TextWriter._
//import java.io._
//object Play {
//
//  def main(args: Array[String]) {
//
//    var dm = DenseMatrix.zeros[Double](6, 6);
//    dm(5, 5) = 3
//    println(dm)
//
//    val psw = new PrintStreamWriter(new PrintStream(new FileOutputStream("../play.txt")))
//    //	val table = new DMTabulizer(dm)
//    //	CSVTableSerialization.write[DMTabulizer[Double]](psw, table);
//
//    val table = new TextTableWriter(psw, ',', '"');
//    for (i <- 0 until dm.rows) {
//      val row = table.next()
//      for (j <- 0 until dm.cols) {
//        val cell = row.next()
//        cell.append(dm(i, j).toString)
//        cell.finish()
//      }
//      row.finish()
//    }
//    table.finish()
//    
//    
//  }
//
//}
//
//class DMTabulizer[V](matrix: Matrix[V])(implicit man: ClassManifest[V]) extends TableWritable[Matrix[V]] {
//  override def header: Option[List[String]] =
//    Some(List("Matrix"))
//
//  override def write(output: TableWriter, value: Matrix[V]) =
//    {
//      for (i <- 0 until matrix.rows) {
//        val row = output.next()
//        for (j <- 0 until matrix.cols) {
//          val cell = row.next()
//          cell.append(matrix(i, j).toString)
//          cell.finish()
//        }
//        row.finish()
//      }
//      output.finish()
//    }
//}