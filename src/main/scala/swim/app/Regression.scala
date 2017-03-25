package swim.app

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import fuel.util.Options
import fuel.util.TRandom
import swim.DomainWithVars
import swim.Grammar
import swim.ProblemProvider
import swim.Tests
import swim.tree.ConstantProviderUniformI
import swim.tree.Op
import scala.collection.Seq

case class FloatingPointDomain(override val numVars: Int) extends DomainWithVars[Seq[Double], Double, Op](numVars) {
  override def semantics(input: Seq[Double]) = {
    assume(input.size == numVars)
    new Function1[Op, Double] {
      def apply(op: Op): Double = {
        val a = op.args
        op.op match {
          case '+        => apply(a(0)) + apply(a(1))
          case '-        => apply(a(0)) - apply(a(1))
          case '*        => apply(a(0)) * apply(a(1))
          case '/        => { val e = apply(a(0)); val d = apply(a(1)); if (d != 0) e / d else 1.0 }
          case 'sin      => math.sin(apply(a(0)))
          case 'cos      => math.cos(apply(a(0)))
          case 'tan      => math.tan(apply(a(0)))
          case 'exp      => math.exp(apply(a(0)))
          case 'lg       => { val x = apply(a(0)); if (x != 0) math.log(math.abs(x)) else 1.0 }
          case 'sig      => 1.0 / (1 + math.exp(-apply(a(0))))
          case 'n        => 1.0/math.sqrt(2*math.Pi) * math.exp({ val r = apply(a(0)); -(r * r / 2) }) // N(0,1)
          case 'neg      => -apply(a(0))
          case i: Int    => input(i)
          case v: Double => v
        }
      }
    }
  }
}

case object FloatingPointDomain {
  val arithm = List('+, '-, '*, '/)
  def instructionSet(name: String) = name match {
    case "default"     => Map(2 -> arithm)
    case "withTransc"  => Map(1 -> List('sin, 'cos, 'exp, 'lg, 'neg), 2 -> arithm)
    case "withNonTrig" => Map(1 -> List('sig, 'exp, 'lg, 'neg, 'n), 2 -> arithm)
  }
}

/* Some popular symbolic regression benchmarks.
 * See http://gpbenchmarks.org/ for more. 
 */
case class RegressionBenchmark(implicit rng: TRandom) extends ProblemProvider[Seq[Double], Double, Op] {
  def apply(conf: Options) = {
    def pr(d: Double) = f"$d%.2f".toDouble
    def g(l: Double, u: Double, s: Double) = l.to(u, s).map(v => pr(v))
    val bench = Map[Any, (Int, Seq[(Seq[Double], Double)])](
      "quartic" -> (1, g(-1, 1, 0.1).map(x => (Seq(x), x * x * x * x + x * x * x + x * x + x))),
      "sext" -> (1, g(-1, 1, 0.1).map(x => (Seq(x), x * x * x * x * x * x - 2.0 * x * x * x * x + x * x))),
      "keij1" -> (1, g(-1, 1, 0.1).map(x => (Seq(x), 0.3 * x * math.sin(2 * math.Pi * x)))),
      "keij4" -> (1, g(0, 10, 0.5).map(x => (Seq(x), x * x * x * math.exp(-x) * math.cos(x) * math.sin(x) * (math.sin(x) * math.sin(x) * math.cos(x) - 1)))),
      "nguy3" -> (1, g(-1, 1, 0.1).map(x => (Seq(x), x * x * x * x * x + x * x * x * x + x * x * x + x * x + x))),
      "nguy4" -> (1, g(-1, 1, 0.1).map(x => (Seq(x), x * x * x * x * x * x + x * x * x * x * x + x * x * x * x + x * x * x + x * x + x))),
      "nguy5" -> (1, g(-1, 1, 0.1).map(x => (Seq(x), math.sin(x * x) * math.cos(x) - 1.0))),
      "nguy6" -> (1, g(-1, 1, 0.1).map(x => (Seq(x), math.sin(x) + math.sin(x * x + x)))),
      "nguy7" -> (1, g(0, 2, 0.1).map(x => (Seq(x), math.log(x + 1) + math.log(x * x + 1.0)))))
    val (v, gg) = bench(conf.paramString("benchmark"))
    val instrSet = FloatingPointDomain.instructionSet(conf.paramString("instructions", "default")) +
      (0 -> List(ConstantProviderUniformI(0, v - 1))) // input variables
    (Grammar.fromSingleTypeInstructions(instrSet), FloatingPointDomain(v), Tests(gg.toIndexedSeq))
  }
}
case object RegressionBenchmark {
  def allBenchmarks = Seq("quartic", "sextic", "keij1", "keij4", "nguy3", "nguy4", "nguy5", "nguy6", "nguy7")
}
