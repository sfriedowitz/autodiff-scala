import spire.math._
import spire.implicits._
import spire.algebra._

import scala.reflect.ClassTag

protected abstract class AutoDiffFunction(val inputSize: Int,
                                          val outputSize: Int) {
  require(inputSize > 0, s"Input size must be positive: $inputSize")
  require(outputSize > 0, s"Output size must be positive: $outputSize")

  implicit val jetDim: JetDim = JetDim(inputSize)
}

abstract class AutoDiffScalarFunction(inputSize: Int)
    extends AutoDiffFunction(inputSize, 1) { self =>

//  lazy private val vectorFunction: AutoDiffVector =
//    new AutoDiffVector(inputSize, inputSize) {
//      override def apply[T: Field: Trig: NRoot: ClassTag](
//        x: Array[T]
//      ): Array[T] = {
//        val g = Array.ofDim[T](x.length)
//        AutoDiff.evaluateGradient(self.apply[Jet[T]], x, g)
//        g
//      }
//    }

  def apply[T: Field: Trig: NRoot: ClassTag](x: Array[T]): T

  def gradient(x: Array[Double]): Array[Double] = {
    AutoDiff.evaluateGradient[Double](this.apply, x)
  }

  def gradient(x: Array[Double], g: Array[Double]): Unit = {
    AutoDiff.evaluateGradient[Double](this.apply, x, g)
  }

}

abstract class AutoDiffVectorFunction(inputSize: Int, outputSize: Int)
    extends AutoDiffFunction(inputSize, outputSize) {

  def apply[T: Field: Trig: NRoot: ClassTag](x: Array[T]): Array[T]

  def jacobian(x: Array[Double]): Array[Array[Double]] = {
    val J = Array.ofDim[Double](inputSize, outputSize)
    AutoDiff.evaluateJacobian[Double](this.apply, x, J)
    J
  }

  def jacobian(x: Array[Double], J: Array[Array[Double]]): Unit = {
    AutoDiff.evaluateJacobian[Double](this.apply, x, J)
  }

}

object AutoDiff {

  def toJets[T: Field: Trig: NRoot: ClassTag](
    x: Array[T]
  )(implicit dim: JetDim): Array[Jet[T]] = {
    val jets = Array.ofDim[Jet[T]](x.length)
    cforRange(x.indices) { i =>
      jets(i) = Jet[T](x(i), i)
    }
    jets
  }

  def evaluateGradient[T: Field: Trig: NRoot: ClassTag](
    func: Array[Jet[T]] => Jet[T],
    x: Array[T],
    g: Array[T]
  )(implicit dim: JetDim): Unit = {
    val jets = toJets(x)
    val res = func(jets)
    cforRange(x.indices) { i =>
      g(i) = res.infinitesimal(i)
    }
  }

  def evaluateGradient[T: Field: Trig: NRoot: ClassTag](
    func: Array[Jet[T]] => Jet[T],
    x: Array[T]
  )(implicit dim: JetDim): Array[T] = {
    val g = Array.ofDim[T](x.length)
    evaluateGradient(func, x, g)
    g
  }

  def evaluateJacobian[T: Field: Trig: NRoot: ClassTag](
    func: Array[Jet[T]] => Array[Jet[T]],
    x: Array[T],
    J: Array[Array[T]]
  )(implicit dim: JetDim): Unit = {
    val jets = toJets(x)
    val res = func(jets)
    cforRange2(x.indices, res.indices) { (i, j) =>
      J(i)(j) = res(j).infinitesimal(i)
    }
  }

}
