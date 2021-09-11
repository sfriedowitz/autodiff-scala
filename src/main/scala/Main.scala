import breeze.linalg.{DenseMatrix, DenseVector}
import spire.algebra._
import spire.implicits._
import spire.math.{Jet, JetDim}

import scala.reflect.ClassTag

object Main extends App {

  case class ScalarFunction(k: Double) extends AutoDiffScalarFunction(2) {
    override def apply[T: Field: Trig: NRoot: ClassTag](x: Array[T]): T = {
      x(0) * x(0) * x(1) + 4 * spire.math.exp(-spire.math.cos(x(1)))
    }
  }

  case class VectorFunction() extends AutoDiffVectorFunction(2, 2) {
    override def apply[T: Field: Trig: NRoot: ClassTag](
      x: Array[T]
    ): Array[T] = {
      Array(x(0) + x(0) * x(1), x(1) * x(1))
    }
  }

  implicit val d: JetDim = JetDim(1)
  val x1 = Jet[Double](0.0, 0)
  val x2 = Jet[Double](4.0, 0)

  val x = Array(0.0, 4.0)
  val g = Array(0.0, 0.0)
  val f = ScalarFunction(5.0)
  f.gradient(x, g)
  println(DenseVector(g: _*))

}
