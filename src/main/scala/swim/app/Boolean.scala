package swim.app

import fuel.util.TRandom
import fuel.util.Options
import scala.collection.Seq
import swim.ProblemProvider
import swim.tree.ConstantProviderUniformI
import swim.DomainWithVars
import swim.tree.Op
import swim.Tests
import swim.Grammar

case class BooleanDomain(override val numVars: Int) extends DomainWithVars[Seq[Boolean], Boolean, Op](numVars) {
  override def semantics(input: Seq[Boolean]) = {
    assume(input.size == numVars)
    new Function1[Op, Boolean] {
      def apply(op: Op): Boolean = {
        val a = op.args
        op.op match {
          case '!         => !apply(a(0))
          case '&         => apply(a(0)) & apply(a(1))
          case '|         => apply(a(0)) | apply(a(1))
          case '&&        => apply(a(0)) && apply(a(1))
          case '||        => apply(a(0)) || apply(a(1))
          case '^         => apply(a(0)) ^ apply(a(1))
          case '!&        => !(apply(a(0)) && apply(a(1)))
          case '!|        => !(apply(a(0)) || apply(a(1)))
          case i: Int     => input(i)
          case v: Boolean => v
        }
      }
    }
  }
}
case object BooleanDomain {
  def instructionSet(name: String) = name match {
    case "and"      => Map(2 -> List('&))
    case "andOrNeg" => Map(1 -> List('!), 2 -> List('&, '|))
    case "default"  => Map(2 -> List('&, '|, '!&, '!|))
    case "withNeg"  => Map(1 -> List('!), 2 -> List('&, '|, '!&, '!|))
  }
}

/* Some popular Boolean function synthesis benchmarks.
 * See http://gpbenchmarks.org/ for more. 
 */
case class BooleanBenchmark(implicit rng: TRandom) extends ProblemProvider[Seq[Boolean], Boolean, Op] {
  def apply(conf: Options) = {
    val benchmarkName = conf.paramString("benchmark")
    val prob = """([a-z]+)([0-9]+)""".r
    val (numVars, testGenerator) = benchmarkName match {
      case prob("empty", n) => (n.toInt, 0.until(1 << n.toInt).map(i => (i, 0)))
      case "mux3"           => (3, 0.until(8).map(i => (i, if ((i & (1 << ((i & 1) + 1))) != 0) 1 else 0)))
      case "mux6"           => (6, 0.until(64).map(i => (i, if ((i & (1 << ((i & 3) + 2))) != 0) 1 else 0)))
      case "mux11"          => (11, 0.until(2048).map(i => (i, if ((i & (1 << ((i & 7) + 3))) != 0) 1 else 0)))
      case "cmp6"           => (6, 0.until(64).map(i => (i, if ((i & 7) < ((i >> 3) & 7)) 1 else 0)))
      case "cmp8"           => (8, 0.until(256).map(i => (i, if ((i & 15) < ((i >> 4) & 15)) 1 else 0)))
      case prob("par", n)   => (n.toInt, 0.until(1 << n.toInt).map(i => (i, i.toBinaryString.count(_.toInt == 49) & 1)))
      case prob("maj", n)   => (n.toInt, 0.until(1 << n.toInt).map(i => (i, if (i.toBinaryString.count(_.toInt == 49) > n.toInt / 2) 0 else 1)))
    }
    val tests = Tests(testGenerator, numVars)
    assert(tests.size == 1 << numVars, "numVars inconsistent with test generator")

    val instrSet = BooleanDomain.instructionSet(conf.paramString("instructions", "default")) +
     (0 -> List(ConstantProviderUniformI(0, numVars - 1))) // input variables
    (Grammar.fromSingleTypeInstructions(instrSet), BooleanDomain(numVars), tests)
  }
}
case object BooleanBenchmark {
  def allBenchmarks = Seq("mux3", "mux6", "mux11", "cmp6", "cmp8", "par5", "maj5")
}