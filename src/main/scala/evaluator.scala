import breeze.linalg.{DenseMatrix, DenseVector}
import spire.math._
import spire.implicits._

trait AutoDiffCache[I, V, D] {
  val func: AutoDiffFunction
  implicit val jetDim: JetDim = JetDim(func.inputSize)

  def evaluate(x: I): Unit

  def getValue: V

  def getDerivative: D

}

case class DerivativeCache(func: AutoDiffUnivariate)
    extends AutoDiffCache[Double, Double, Double] {

  private var result: Jet[Double] = Jet.zero

  def evaluate(x: Double): Unit = result = func(Jet(x, 0))

  def getValue: Double = result.real

  def getDerivative: Double = result.infinitesimal(0)

}

case class GradientCache(func: AutoDiffMultivariate)
    extends AutoDiffCache[DenseVector[Double], Double, DenseVector[Double]] {

  private var result: Jet[Double] = Jet.zero
  private val jets = DenseVector.zeros[Jet[Double]](func.inputSize)
  private val gradient = DenseVector.zeros[Double](func.inputSize)

  def evaluate(x: DenseVector[Double]): Unit = {
    cforRange(0 until x.size) { i =>
      jets(i) = Jet[Double](x(i), i)
    }
    result = func(jets)
    cforRange(0 until x.size) { i =>
      gradient(i) = result.infinitesimal(i)
    }
  }

  def getValue: Double = result.real

  def getDerivative: DenseVector[Double] = gradient

}

case class JacobianCache(func: AutoDiffVector)
    extends AutoDiffCache[DenseVector[Double], DenseVector[Double], DenseMatrix[
      Double
    ]] {

  private val value = DenseVector.zeros[Jet[Double]](func.inputSize)
  private val jets = DenseVector.zeros[Jet[Double]](func.inputSize)
  private val jacobian: DenseMatrix[Double] =
    DenseMatrix.zeros[Double](func.inputSize, func.outputSize)

  def evaluate(x: DenseVector[Double]): Unit = {
    cforRange(0 until func.inputSize) { i =>
      jets(i) = Jet[Double](x(i), i)
    }
    func(jets, value)
    cforRange2(0 until func.inputSize, 0 until func.outputSize) { (i, j) =>
      jacobian(i, j) = value(j).infinitesimal(i)
    }
  }

  def getValue: DenseVector[Double] = value.map(_.real)

  def getDerivative: DenseMatrix[Double] = jacobian

}
