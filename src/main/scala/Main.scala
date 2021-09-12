import breeze.linalg.{DenseVector, norm}
import breeze.optimize.{DiffFunction, LBFGS}

import scala.{specialized => sp}
import spire.implicits._
import spire.algebra.{Field, Trig, NRoot}

import scala.reflect.ClassTag

object Main extends App {

  case class NormFunctor() extends AutoDiffMultivariate(1) {
    override def apply[@sp(Double) T: Field: Trig: NRoot: ClassTag](
      x: DenseVector[T]
    ): T = {
      val t = implicitly[Trig[T]]
      t.cos(x(0))
    }
  }

  def measure(f: => Unit): Double = {
    val start1 = System.nanoTime
    f
    (System.nanoTime - start1) / 1e9
  }

  val func = NormFunctor()
  val x0 = DenseVector(math.Pi)

  val n = 10000
  val avgGeneric =
    (1 to n)
      .map(
        _ =>
          measure {
            func.evaluate(x0); val grad = func.getGradient;
        }
      )
      .sum / n
  println(avgGeneric)

}
