import spire.math._
import spire.implicits._
import spire.algebra._

import breeze.linalg.{DenseVector, DenseMatrix}

import scala.reflect.ClassTag

abstract class AutoDiffFunction(val inputSize: Int, val outputSize: Int) {
  require(inputSize > 0, s"Input size must be positive: $inputSize")
  require(outputSize > 0, s"Output size must be positive: $outputSize")

  implicit val jetDim: JetDim = JetDim(inputSize)

}

abstract class VectorAutoDiffFunction(inputSize: Int, outputSize: Int)
    extends AutoDiffFunction(inputSize, outputSize) {

  def apply[T: Field: Trig: NRoot: ClassTag](
      x: DenseVector[T]
  ): DenseVector[T]

  def jacobian(x: DenseVector[Double]): DenseMatrix[Double] = {
    val J = DenseMatrix.zeros[Double](inputSize, outputSize)
    AutoDiff.evaluateJacobian(this, x, J)(this.jetDim)
    J
  }

  def jacobian(x: DenseVector[Double], J: DenseMatrix[Double]): Unit = {
    AutoDiff.evaluateJacobian(this, x, J)(this.jetDim)
  }

}

abstract class ScalarAutoDiffFunction(inputSize: Int)
    extends AutoDiffFunction(inputSize, 1) { self =>

//  lazy private val vectorFunction =
//    new VectorAutoDiffFunction(inputSize, inputSize) {
//      override def apply[T: Field: Trig: NRoot: ClassTag](
//          x: DenseVector[T]
//      ): DenseVector[T] = {
//        self.gradient(x)
//      }
//    }

  def apply[T: Field: Trig: NRoot: ClassTag](
      x: DenseVector[T]
  ): T

  def gradient(x: DenseVector[Double]): DenseVector[Double] = {
    val g = DenseVector.zeros[Double](inputSize)
    gradient(x, g)
    g
  }

  def gradient(x: DenseVector[Double], g: DenseVector[Double]): Unit = {
    AutoDiff.evaluateGradient(this, x, g)(this.jetDim)
  }

}

private object AutoDiff {

  def evaluateGradient(
      func: ScalarAutoDiffFunction,
      x: DenseVector[Double],
      g: DenseVector[Double]
  )(implicit dim: JetDim): Unit = {
    val m = func.inputSize

    val jets = DenseVector.zeros[Jet[Double]](m)
    cforRange(0 until m) { i =>
      jets(i) = x(i) + Jet.h[Double](i)
    }

    val res = func(jets)
    cforRange(0 until m) { i =>
      g(i) = res.infinitesimal(i)
    }
  }

  def evaluateJacobian(
      func: VectorAutoDiffFunction,
      x: DenseVector[Double],
      J: DenseMatrix[Double]
  )(implicit dim: JetDim): Unit = {
    val m = func.inputSize
    val n = func.outputSize

    val jets = DenseVector.zeros[Jet[Double]](m)
    cforRange(0 until m) { j =>
      jets(j) = Jet[Double](x(j), j)
    }

    val res = func(jets)
    cforRange2(0 until m, 0 until n) { (i, j) =>
      J(i, j) = res(i).infinitesimal(j)
    }
  }

}
