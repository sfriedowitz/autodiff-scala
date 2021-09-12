import breeze.linalg.{DenseMatrix, DenseVector}

import scala.{specialized => sp}
import spire.algebra._
import spire.implicits._

import scala.reflect.ClassTag

protected abstract class AutoDiffFunction(val inputSize: Int,
                                          val outputSize: Int) {
  require(inputSize > 0, s"Input size must be positive: $inputSize")
  require(outputSize > 0, s"Output size must be positive: $outputSize")
}

abstract class AutoDiffUnivariate extends AutoDiffFunction(1, 1) {

  private val cache = DerivativeCache(this)

  def apply[@sp(Double) T: Field: Trig: NRoot: ClassTag](x: T): T

  def evaluate(x: Double): Unit = cache.evaluate(x)

  def getValue: Double = cache.getValue

  def getDerivative: Double = cache.getDerivative

}

abstract class AutoDiffMultivariate(inputSize: Int)
    extends AutoDiffFunction(inputSize, 1) {

  private val cache = GradientCache(this)

  def apply[T: Field: Trig: NRoot: ClassTag](x: DenseVector[T]): T

  def evaluate(x: DenseVector[Double]): Unit = cache.evaluate(x)

  def getValue(x: DenseVector[Double]): Double = cache.getValue

  def getGradient: DenseVector[Double] = cache.getDerivative

}

abstract class AutoDiffVector(inputSize: Int, outputSize: Int)
    extends AutoDiffFunction(inputSize, outputSize) {

  private val cache = JacobianCache(this)

  def apply[T: Field: Trig: NRoot: ClassTag](x: DenseVector[T],
                                             F: DenseVector[T]): Unit

  def evaluate(x: DenseVector[Double]): Unit = cache.evaluate(x)

  def getValue: DenseVector[Double] = cache.getValue

  def getGradient: DenseMatrix[Double] = cache.getDerivative

}
