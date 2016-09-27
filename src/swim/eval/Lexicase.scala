package swim.eval

import scala.annotation.tailrec
import fuel.util.TRandom
import fuel.func.StochasticSelection
import fuel.Preamble.RndApply

/**
  * Lexicase selection by Spector et al. Applicable to test-based problems.
  *
  * Iteratively selects a random test and keeps only the solutions that pass it.
  * It does so until only one solution is left, and that solution is the outcome of selection.
  *
  * Note: Here E stands for one objective, not entire evaluation.
  * 
  * For details on lexicase seleciton, see: 
  * Helmuth, T.; Spector, L. & Matheson, J., "Solving Uncompromising Problems with 
  * Lexicase Selection", IEEE Transactions on Evolutionary Computation, 2015, 19, 630-643
  * 
  */
class LexicaseSelection[S, E](o: Ordering[E])(implicit rand: TRandom)
    extends StochasticSelection[S, Seq[E]](rand) {
  def apply(pop: Seq[(S, Seq[E])]) = {
    @tailrec def sel(sols: Seq[(S, Seq[E])], cases: IndexedSeq[Int]): (S, Seq[E]) =
      if (sols.size == 1) sols(0)
      else if(cases.isEmpty) sols(rand)
      else {
        val theCase = cases.head
        val ord = new Ordering[(S, Seq[E])] {
          override def compare(a: (S, Seq[E]), b: (S, Seq[E])) = o.compare(a._2(theCase), b._2(theCase))
        }
        val best = sols.min(ord)
        sel(sols.filter(s => ord.compare(s, best) <= 0), cases.tail)
      }
    // assumes nonempty pop
    val t = rand.shuffle(0.until(pop(0)._2.size).toIndexedSeq)
    val r = sel(pop, t)
    r
  }
}


/** EXPERIMENTAL
 *  TODO: E is dummy here; ideally, this should be rewritten without E whatsoever, 
 *  but this requires redefinition of Seleciton trait
 */

class LexicaseSelectionNoEval[S,E](o: Seq[Ordering[S]])(implicit rand: TRandom)
    extends StochasticSelection[S,Seq[E]](rand) {
  def apply(pop: Seq[(S, Seq[E])]) = {
    @tailrec def sel(sols: Seq[(S,Seq[E])], cases: Seq[Ordering[S]]): (S,Seq[E]) =
      if (sols.size == 1) sols(0)
      else if(cases.isEmpty) sols(rand)
      else {
        val ord = cases(rand)
        val best = sols.minBy(_._1)(ord)
        sel(sols.filter(s => ord.compare(s._1, best._1) <= 0), cases.diff(List(ord)))
      }
    // assumes nonempty pop
    sel(pop, o)
  }
}
