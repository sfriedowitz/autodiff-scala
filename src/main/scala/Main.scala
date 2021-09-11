import breeze.linalg.DenseVector

import spire.algebra._
import spire.implicits._

import scala.reflect.ClassTag

object Main extends App {

  case class SimpleFunction(k: Double) extends ScalarAutoDiffFunction(2) {
    override def apply[T: Field: Trig: NRoot: ClassTag](
        x: DenseVector[T]
    ): T = {
      x(0) * x(0) * x(1) + k * Trig[T].cos(x(0) * x(1))
    }
  }

  val x = DenseVector(4.0, 2.0)
  val f = SimpleFunction(10.0)

  println(f.gradient(x))

}
