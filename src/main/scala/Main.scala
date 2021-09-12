import breeze.linalg.{DenseVector, norm}
import breeze.optimize.{
  DiffFunction,
  EmpiricalHessian,
  LBFGS,
  OWLQN,
  SecondOrderFunction,
  TruncatedNewtonMinimizer
}

import scala.{specialized => sp}
import spire.algebra.{Field, NRoot, Trig}
import spire.implicits._
import spire.math._

import scala.reflect.ClassTag
import scala.util.Random

object Main extends App {

  def measureTime(f: => Unit): Double = {
    val start1 = System.nanoTime
    f
    (System.nanoTime - start1) / 1e9
  }

  case class NormFunc() extends MultivariateAutoDiffFunction(3) {
    override def apply[@sp(Double) T: Field: Trig: NRoot: ClassTag](
        x: DenseVector[T]
    ): T = {
      val f = implicitly[Field[T]]
      f.pow(x(0) - 3.0, 2) + f.pow(x(1) - 4.0, 2) + spire.math.exp(x(2) * x(2))
    }
  }

  case class GaussianFunc(data: Vector[Double])
      extends MultivariateAutoDiffFunction(2) {

    def pdf[@sp(Double) T: Field: Trig: NRoot: ClassTag](
        x: Double,
        mu: T,
        sigma: T
    ): T = {
      val t = implicitly[Trig[T]]
      val z = (x - mu) / sigma
      (1.0 / sigma / math.sqrt(2 * math.Pi)) * t.exp(-0.5 * z * z)
    }

    override def apply[@sp(Double) T: Field: Trig: NRoot: ClassTag](
        params: DenseVector[T]
    ): T = {
      val f = implicitly[Field[T]]
      val t = implicitly[Trig[T]]

      var sum = f.zero
      for (x <- data) {
        sum += -t.log(pdf(x, params(0), params(1)))
      }
      sum
    }
  }

  val x0 = DenseVector(1.0, 1.0)
  val lbfgs = new LBFGS[DenseVector[Double]](50, 5)
  val newton = new TruncatedNewtonMinimizer[DenseVector[
    Double
  ], EmpiricalHessian[DenseVector[Double]]](100, tolerance = 1e-8)

  val f: DiffFunction[DenseVector[Double]] = {
    new DiffFunction[DenseVector[Double]] {
      val data: Vector[Double] =
        Vector.fill(64)(5.0 + 10.0 * Random.nextGaussian)
      val func: GaussianFunc = GaussianFunc(data)
      def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        func.evaluate(x)
        (func.getValue, func.getGradient + 1e-10)
      }
    }
  }
  val emp = SecondOrderFunction.empirical(f)

//
//  val result = lbfgs.minimize(f, x0)
//  println(result)

  val n = 500
  val avgGeneric =
    (1 to n).map(_ => measureTime { lbfgs.minimize(f, x0) }).sum / n
  println(avgGeneric)

}
