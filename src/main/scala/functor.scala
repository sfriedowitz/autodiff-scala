import breeze.linalg.{DenseMatrix, DenseVector}

import scala.{specialized => sp}
import spire.algebra._
import spire.implicits._

import scala.reflect.ClassTag

protected abstract class AutoDiffFunctor(
    val inputSize: Int,
    val outputSize: Int
) {
  require(inputSize > 0, s"Input size must be positive: $inputSize")
  require(outputSize > 0, s"Output size must be positive: $outputSize")
}

abstract class UnivariateFunctor extends AutoDiffFunctor(1, 1) {

  private val evaluator = DerivativeEvaluator(this)

  def apply[@sp(Double) T: Field: Trig: NRoot: ClassTag](x: T): T

  def getValue(x: Double): Double = apply(x)

  def getDerivative(x: Double): Double = evaluator.evaluateDerivative(x)

}

abstract class ScalarFunctor(inputSize: Int)
    extends AutoDiffFunctor(inputSize, 1) {

  private val gradientEvaluator = GradientEvaluator(this)

  def apply[T: Field: Trig: NRoot: ClassTag](x: DenseVector[T]): T

  def getValue(x: DenseVector[Double]): Double = apply(x)

  def getGradient(x: DenseVector[Double]): DenseVector[Double] =
    gradientEvaluator.evaluateGradient(x)

  def getHessian(x: DenseVector[Double]): DenseMatrix[Double] = ???

}

abstract class VectorFunctor(inputSize: Int, outputSize: Int)
    extends AutoDiffFunctor(inputSize, outputSize) {

  private val evaluator = JacobianEvaluator(this)

  def apply[T: Field: Trig: NRoot: ClassTag](
      x: DenseVector[T],
      F: DenseVector[T]
  ): Unit

  def getValue(x: DenseVector[Double], F: DenseVector[Double]): Unit =
    apply(x, F)

  def getValue(x: DenseVector[Double]): DenseVector[Double] = {
    val F = DenseVector.zeros[Double](x.size)
    getValue(x, F)
    F
  }

  def getJacobian(x: DenseVector[Double]): DenseMatrix[Double] =
    evaluator.evaluateJacobian(x)

}
