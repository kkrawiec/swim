package swim.app

import fuel.util.Combinations
import fuel.util.TRandom
import fuel.util.CodeExecutor
import fuel.util.Options
import swim.ProblemProvider
import swim.tree.ConstantProviderUniformI
import swim.DomainWithVars
import swim.tree.Op
import swim.Test
import swim.Grammar


/* The domain for solving certain class of problems in finite algebras, 
 * as defined in: 
 * 
 * L. Spector, D. M. Clark, I. Lindsay, B. Barr, and
 * J. Klein. Genetic programming for finite algebras. 
 * In M. Keijzer, et al., editors, GECCO ’08: Proceedings of the 10th annual conference 
 * on Genetic and evolutionary computation, pages 1291–1298, 
 * Atlanta, GA, USA, 12-16 July 2008. ACM.
 * 
 */

case class AlgebraDomain(override val numVars: Int, val algebra: Array[Array[Int]])
    extends DomainWithVars[Seq[Int], Int, Op](numVars) {
  assume(algebra.nonEmpty)
  assume(algebra.forall(_.size == algebra.size))

  override def semantics(input: Seq[Int]) = {
    assume(input.size == numVars)
    new Function1[Op, Int] {
      def apply(op: Op): Int = {
        val a = op.args
        op.op match {
          case 'a     => algebra(apply(a(0)))(apply(a(1)))
          case i: Int => input(i)
        }
      }
    }
  }
}

object AlgebraDomain {
  def apply(numVars: Int, spec: String) = {
    val a = spec.split(",").toList.map(_.toInt)
    val s = math.sqrt(a.size)
    assume(s.isWhole, "There should be k^2 elements in algebra definition")
    val algebra = a.grouped(s.toInt).map(_.toArray).toArray
    println(algebra.deep.mkString("\n"))
    new AlgebraDomain(numVars, algebra)
  }
}

object AlgebraTask {
  def discriminator(i: Seq[Int]) = Some(if (i(0) != i(1)) i(0) else i(2))
  def malcev(i: Seq[Int]) = if (i(0) == i(1)) Some(i(2)) else if (i(1) == i(2)) Some(i(0)) else None
  def apply(termSpec: String, numVars: Int): Seq[Test[Seq[Int], Int]] = {
    val term = CodeExecutor(s"""scaps.app.AlgebraTask.${termSpec} _""").asInstanceOf[Seq[Int] => Option[Int]]
    apply(term, numVars)
  }
  def apply(term: (Seq[Int] => Option[Int]), numVars: Int) = {
    assert(numVars == 3) // at least for now
    Combinations(numVars, numVars).map(
      input => term(input).map(desiredOut => Test[Seq[Int], Int](input.toIndexedSeq, desiredOut))).flatten.toIndexedSeq
  }
}

case class AlgebraBenchmark(implicit rng: TRandom) extends ProblemProvider[Seq[Int], Int, Op] {
  def apply(conf: Options) = {
    val algebras = List(
      "2,1,2,1,0,0,0,0,1",
      "2,0,2,1,0,2,1,2,1",
      "1,0,1,1,2,0,0,0,0",
      "1,0,1,0,2,0,0,1,0",
      "1,0,2,1,2,0,0,1,0")
    val benchmarkName = conf.paramString("benchmark")
    val prob = """([a-z]+)([0-9]+)""".r
    val grammar = Grammar.fromSingleTypeInstructions(0 -> List(ConstantProviderUniformI(0, 2)), 2 -> List('a))
    benchmarkName match {
      case prob("malcev", algId) if algId.toInt > 0 && algId.toInt <= algebras.size =>
        (grammar, AlgebraDomain(3, algebras(algId.toInt - 1)), AlgebraTask(AlgebraTask.malcev _, 3))
      case prob("disc", algId) if algId.toInt > 0 && algId.toInt <= algebras.size =>
        (grammar, AlgebraDomain(3, algebras(algId.toInt - 1)), AlgebraTask(AlgebraTask.discriminator _, 3))
    }
  }
}

