import breeze.linalg.{DenseVector, norm}
import breeze.optimize.{DiffFunction, LBFGS}

import scala.{specialized => sp}
import spire.implicits._
import spire.algebra._
import spire.math._

import scala.reflect.ClassTag
import scala.util.Random

object Main extends App {

  case class NormFunctor() {

    def eval[T: Field: NRoot: Trig](
        x: DenseVector[T]
    ): T = {
      (x(0) - 3.0) * (x(1) - 3.0)
    }
  }

  def f(x: DenseVector[Double]): Double = {
    (x(0) - 3.0) * (x(1) - 3.0)
  }

  def measure(f: => Unit): Long = {
    val start1 = System.nanoTime
    f
    System.nanoTime - start1
  }

  val functor = NormFunctor()
  val x0 = DenseVector(3.0, 3.0)

  val n = 10000000
  val avgGeneric =
    (1 to n).map(_ => measure { val out = functor.eval(x0) }).sum / n
  println(avgGeneric)

  val avgNorm = (1 to n).map(_ => measure { val out = f(x0) }).sum / n
  println(avgNorm)

}
