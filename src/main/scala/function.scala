import breeze.linalg.{DenseMatrix, DenseVector}

import scala.{specialized => sp}
import spire.algebra.{Field, Trig, NRoot}
import spire.math._

import scala.reflect.ClassTag

abstract class AutoDiffFunction(val inputSize: Int, val outputSize: Int) {
  require(inputSize > 0, s"Input size must be positive: $inputSize")
  require(outputSize > 0, s"Output size must be positive: $outputSize")
}

abstract class UnivariateAutoDiffFunction extends AutoDiffFunction(1, 1) {

  private val cache = DerivativeCache(this)

  def apply[@sp(Double) T: Field: Trig: NRoot: ClassTag](x: T): T

  def evaluate(x: Double): Unit = cache.evaluate(x)

  def getValue: Double = cache.getValue

  def getDerivative: Double = cache.getDerivative

}

abstract class MultivariateAutoDiffFunction(inputSize: Int)
    extends AutoDiffFunction(inputSize, 1) {

  private val cache = GradientCache(this)

  def apply[@sp(Double) T: Field: Trig: NRoot: ClassTag](x: DenseVector[T]): T

  def evaluate(x: DenseVector[Double]): Unit = cache.evaluate(x)

  def getValue: Double = cache.getValue

  def getGradient: DenseVector[Double] = cache.getDerivative

}

abstract class VectorAutoDiffFunction(inputSize: Int, outputSize: Int)
    extends AutoDiffFunction(inputSize, outputSize) {

  private val cache = JacobianCache(this)

  def apply[@sp(Double) T: Field: Trig: NRoot: ClassTag](
      x: DenseVector[T],
      F: DenseVector[T]
  ): Unit

  def evaluate(x: DenseVector[Double]): Unit = cache.evaluate(x)

  def getValue: DenseVector[Double] = cache.getValue

  def getGradient: DenseMatrix[Double] = cache.getDerivative

}
