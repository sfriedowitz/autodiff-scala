import scala.{specialized => sp}
import spire.implicits._
import spire.algebra._

import scala.reflect.ClassTag

object Main extends App {

  case class Parabola() extends UnivariateFunctor {
    override def apply[@sp(Double) T: Field: Trig: NRoot: ClassTag](
        x: T
    ): T =
      (x - 5.0) * (x - 5.0)
  }

  val f = Parabola().getEvaluator

  var x0 = 20.0
  val fx0 = f.functor(x0)

}
