package swim.eval

import fuel.Preamble.iverson
import fuel.core.StatePop
import fuel.func.Evaluation
import swim.Domain
import swim.Program
import swim.Test

/**
  * Implicit fitness sharing, as in:
  *
  * R.E. Smith, S. Forrest, and A.S. Perelson. “Searching for diverse,
  * cooperative populations with genetic algorithms”. In: Evolutionary Computation 1.2 (1993).
  *
  * R I (Bob) McKay. “Fitness Sharing in Genetic Programming”. In:
  * Proceedings of the Genetic and Evolutionary Computation Conference (GECCO-2000).
  * Ed. by Darrell Whitley et al. Las Vegas, Nevada, USA: Morgan Kaufmann, Oct. 2000, pp. 435–442. isbn: 1-55860-708-0.
  * http://www.cs.bham.ac.uk/~wbl/biblio/gecco2000/GP256.ps.
  *
  * IFS evaluation is contextual, i.e., must be applied to the entire population simultaneously.
  *
  * Note: IFS fitness is *maximized*.
  *
  * IFS fitness is not objective, so to monitor progress, we need the conventional fitness as well.
  * Therefore, the IFSFitness stores both types of fitness: the ifs field is used only for selection.
  *
  */

trait IFSEval {
  // A pair: IFSFitness, conventional fitness
  case class Fitness(ifs: Double, numFailedTests: Double)
  class Ord(implicit o: Ordering[Double]) extends Ordering[Fitness] {
    override def compare(x: Fitness, y: Fitness) = o.compare(y.ifs, x.ifs)
  }
  def apply[I, O, P<:Program](tests: Seq[Test[I, O]],
                  domain: Domain[I,O,P], 
                  interact: Function2[O, O, Boolean] = (a: O, b: O) => a == b ) =
    new Evaluation[P, Fitness] {
      override def apply(s: StatePop[P]) = {
        val testOutcomes = s.map(p => tests.map(t => iverson(interact(domain(p)(t._1), t._2))))
        val numSolving = testOutcomes.transpose.map(_.sum)
        StatePop(0.until(s.size).map(i => (s(i), {
          val solved = 0.until(tests.size).filter(j => testOutcomes(i)(j) == 1)
          Fitness(solved.map(j => 1.0 / numSolving(j)).sum, tests.size - solved.size)
        })))
      }
    }
}
object IFSEval extends IFSEval

/*
  // My variant of IFS that is applicable to non-binary, ordinal interaction outcomes
  // Whether a program passed a test is determined based on the ranking of the test on this program's output
  // WARNING: Works only for binary interaction outcomes!
  trait EvaluatorOrdinal[P <: Program[I, Double], I]
    extends fuel.mixin.Evaluator[P, BinaryTestOutcomes] {
    this: Tests[I, Double] =>
    lazy val nneg = tests.count(_._2 <= 0)

    override def apply(l: Seq[P]) = {
      val outs = l.map(p => tests.map(t => (p(t._1), t._2))).toVector
      val ranks = outs.map(o => {
        val sor = 0.until(tests.size).sorted(new Ordering[Int] {
          def compare(a: Int, b: Int) = {
            val r = o(a)._1 compare o(b)._1
            if (r != 0) r
            else
              o(b)._2 compare o(a)._2
          }
        })
        val pos = sor.zip(0.until(sor.size)).sorted(new Ordering[(Int, Int)] {
          def compare(a: (Int, Int), b: (Int, Int)) = a._1 compare b._1
        }).map(_._2).toVector
        //      if (o(sor.head)._1 == o(sor.last)._1)
        //        println(pos)
        pos
      }).toVector
      0.until(l.size).map(i => ESol(l(i), new BinaryTestOutcomes(
        0.until(tests.size).map(j => ScalarEvaluationMax( // tests
          // this is the difficulty measure:     
          if (ranks(i)(j) < nneg)
            if (tests(j)._2 <= 0) 1 else 0
          else if (tests(j)._2 > 0) 1 else 0)))))
    }
  }

*/