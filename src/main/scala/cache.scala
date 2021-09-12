import breeze.linalg.{DenseMatrix, DenseVector}
import spire.math._
import spire.implicits._

trait AutoDiffCache {
  val func: AutoDiffFunction
  implicit val jetDim: JetDim = JetDim(func.inputSize)
}

case class DerivativeCache(func: UnivariateAutoDiffFunction)
    extends AutoDiffCache {

  private var value: Jet[Double] = Jet.zero

  def evaluate(x: Double): Unit = value = func(Jet(x, 0))

  def getValue: Double = value.real

  def getDerivative: Double = value.infinitesimal(0)

}

case class GradientCache(func: MultivariateAutoDiffFunction)
    extends AutoDiffCache {

  private var value: Jet[Double] = Jet.zero
  private val jets = DenseVector.zeros[Jet[Double]](func.inputSize)
  private val gradient = DenseVector.zeros[Double](func.inputSize)

  def evaluate(x: DenseVector[Double]): Unit = {
    for (i <- 0 until func.inputSize) {
      jets(i) = Jet(x(i), i)
    }
    value = func(jets)
    for (i <- 0 until func.inputSize) {
      gradient(i) = value.infinitesimal(i)
    }
  }

  def getValue: Double = value.real

  def getDerivative: DenseVector[Double] = gradient

}

case class JacobianCache(func: VectorAutoDiffFunction) extends AutoDiffCache {

  private val jets = DenseVector.zeros[Jet[Double]](func.inputSize)
  private val jetValue = DenseVector.zeros[Jet[Double]](func.outputSize)
  private val realValue = DenseVector.zeros[Double](func.outputSize)
  private val jacobian: DenseMatrix[Double] =
    DenseMatrix.zeros[Double](func.inputSize, func.outputSize)

  def evaluate(x: DenseVector[Double]): Unit = {
    for (i <- 0 until func.inputSize) {
      jets(i) = Jet(x(i), i)
    }
    func(jets, jetValue) // Stores value output in jet array
    for (j <- 0 until func.outputSize) {
      realValue(j) = jetValue(j).real
      for (i <- 0 until func.outputSize) {
        jacobian(i, j) = jetValue(j).infinitesimal(i)
      }
    }
  }

  def getValue: DenseVector[Double] = realValue

  def getDerivative: DenseMatrix[Double] = jacobian

}
