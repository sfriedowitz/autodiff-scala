import breeze.linalg.{DenseMatrix, DenseVector}
import spire.math._
import spire.implicits._

trait AutoDiffEvaluator {
  val functor: AutoDiffFunctor
  implicit val jetDim: JetDim = JetDim(functor.inputSize)
}

case class DerivativeEvaluator(functor: UnivariateFunctor)
    extends AutoDiffEvaluator {
  def evaluateDerivative(x: Double): Double =
    functor(Jet(x, 0)).infinitesimal(0)
}

case class GradientEvaluator(functor: MultivariateFunctor)
    extends AutoDiffEvaluator {

  private val jetStorage = DenseVector.zeros[Jet[Double]](functor.inputSize)
  private val gradientStorage = DenseVector.zeros[Double](functor.inputSize)

  def evaluateGradient(x: DenseVector[Double]): DenseVector[Double] = {
    cforRange(0 until x.size) { i =>
      jetStorage(i) = Jet[Double](x(i), i)
    }
    val result = functor(jetStorage)
    cforRange(0 until x.size) { i =>
      gradientStorage(i) = result.infinitesimal(i)
    }
    gradientStorage
  }

}

case class JacobianEvaluator(functor: VectorFunctor) extends AutoDiffEvaluator {

  private val jetStorage = DenseVector.zeros[Jet[Double]](functor.inputSize)
  private val valueStorage = DenseVector.zeros[Jet[Double]](functor.inputSize)
  private val jacobianStorage: DenseMatrix[Double] =
    DenseMatrix.zeros[Double](functor.inputSize, functor.outputSize)

  def evaluateJacobian(x: DenseVector[Double]): DenseMatrix[Double] = {
    val m = functor.inputSize
    val n = functor.outputSize
    cforRange(0 until m) { i =>
      jetStorage(i) = Jet[Double](x(i), i)
    }
    functor(jetStorage, valueStorage)
    cforRange2(0 until m, 0 until n) { (i, j) =>
      jacobianStorage(i, j) = valueStorage(j).infinitesimal(i)
    }
    jacobianStorage
  }

}
