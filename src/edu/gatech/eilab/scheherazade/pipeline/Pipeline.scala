package edu.gatech.eilab.scheherazade.pipeline

//trait Connectable[A] {
//  val name:String
//  def stats():String
//  def ->[B] (c:Connectable[B]):Connectable[B]
//  def apply(): => A
//}

/** This explains the type variance here:
 *  Suppose TypeA is the superclass of TypeB
 *  A pipeline that takes in TypeB is not a subclass of a pipeline that takes in TypeA.
 *  If you can take in TypeB, there is no guarantee that you can take in TypeA as well. So the input must be contra-variant
 *  For implementation, I left out the variant on the input but put it in the source variable. Otherwise Scala would not compile it.
 *  This is an implementation detail which should not affect the correctness of the program
 *  On the other hand, a pipeline that outputs TypeB is a subtype of a pipeline that outputs TypeA.
 *
 *  Albert
 */
abstract class Pipeline[-I, +O] //extends Function1[I, O] with Connectable[O]
{
  val name: String
  var cascadedName: String = ""
  def stats(): String = ""
  def apply(i: I): O

  def ->[X](seg: Pipeline[O, X]): Pipeline[I, X] = {
    val func = this.apply _
    val outerName = this.name
    val outerStats = this.stats
    new Pipeline[I, X] {
      val name = outerName + "." + seg.name
      seg.cascadedName = name
      def apply(input: I): X = seg(func(input))
      override def stats = outerStats + "; " + seg.stats
    }
  }

  //  override def andThen[A](g: O => A) : Pipeline[I, A] = g match {
  //    case p:Pipeline[O, A] => this -> p
  //    case g:(O => A) => this -> Pipeline(g)
  //  }

  //  override def compose[A](g: => I) : Pipeline[A, O] = g match {
  //    case p:Pipeline[A, I] => p -> this
  //    case p:Source[I] => p -> this
  //    case p:Pipeline[A, I] => p -> this
  //    case g:(A => I) => Pipeline(g) -> this
  //  }

  def paral[A, B](seg: Pipeline[A, B]): Pipeline[(I, A), (O, B)] =
    new ParallelPipeline[I, A, O, B](this, seg)

}

object Pipeline {
  /** creates an anonymous pipeline
   *
   */
  def apply[A, B](f: A => B) = new Pipeline[A, B] {
    val name = ""
    def apply(input: A) = f(input)
  }
}

class ParallelPipeline[-I, -J, +O, +P](p1: Pipeline[I, O], p2: Pipeline[J, P]) extends Pipeline[(I, J), (O, P)] {
  val name = p1.name + "||" + p2.name
  def apply(x: (I, J)): (O, P) = {
    val o1 = p1(x._1)
    val o2 = p2(x._2)
    (o1, o2)
  }
  override def stats = "(" + p1.stats + ", " + p2.stats + ")"

}

object ParallelPipeline {
  /** the following implicit conversion does not work.
   *  what's wrong?
   */
  implicit def simplifyUnit[A, B](pp: ParallelPipeline[Unit, Unit, A, B]) =
    new Pipeline[Unit, (A, B)] {
      val name = pp.name
      override def stats = pp.stats
      def apply(i: Unit) = pp((), ())
    }
}

abstract class Source[+O] extends Pipeline[Unit, O] //extends Function0[O] with Connectable[O]
{
  //  override def ->[X](seg: Pipeline[O, X]): Source[X] = {
  //    val func = this.apply _
  //    val outerName = this.name
  //    val outerStats = this.stats
  //    new Source[X] {
  //      val name = outerName + "." + seg.name
  //      def apply(): X = seg(func())
  //      def stats = outerStats + "; " + seg.stats
  //    }
  //  }
}
//{
//  val name: String
//  def produce: () => O
//  def stats(): String

//  def ->[X](seg: Pipeline[O, X]): Source[X] = {
//    val func:() => O = this.produce
//    val outerName = this.name
//    val outerStats = this.stats
//    new Source[X] {
//      val name = outerName + "." + seg.name
//      def produce:() => X = () => seg.produce(func()) 
//      def stats = outerStats + "; " + seg.stats
//    }
//  }
//}

class RandomInteger extends Source[Int] {
  override val name = "randInt"

  //override def apply: () => Int = () => scala.math.round(scala.math.random.asInstanceOf[Float] * 10)
  def apply(u: Unit): Int = scala.math.round(scala.math.random.asInstanceOf[Float] * 10)

  override def stats() = "stateless"
}

class TimesTen() extends Pipeline[Int, Int] {
  private var count = 0
  override val name = "times10"
  def apply(i: Int): Int = {
    count += 1
    i * 10
  }

  override def stats() = "called for " + count + " times"
}

//object TimesTen {
//  // this code achieves the desired connection using ::
//  // but this has to be repeated in each pipeline subclass. 
//  // how to remove this boilerplate code? 
//  def ::(that: Pipeline[_, Int]) = new TimesTen(that)
//
//}

//class Pipeline(list:List[Segment[_, Any]]) {
//  def -> (seg:Segment[_, Any]) = 
//  {
//    new Pipeline(list ::: List(seg))
//  }
//  
//  def produce() = list.map(_.produce).reduce{_ andThen _}
//}

object Runner extends App {
  //val t = new TimesTen()
  val p = new RandomInteger() -> new TimesTen()
  val p2 = new ParallelPipeline(p, new RandomInteger())
  for (i <- 0 to 10)
    println(p())

  println(p.name)
  println(p.stats())
  //println(simplifyUnit(p2).produce())
  println(p2(((), ())))
  //println(p2())
  println("wowo")
}