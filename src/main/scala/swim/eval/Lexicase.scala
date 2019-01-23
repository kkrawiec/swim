package swim.eval

import scala.collection.Seq
import scala.annotation.tailrec
import fuel.util.{TRandom, Utils}
import fuel.func.StochasticSelection
import fuel.Preamble.RndApply

import scala.collection.mutable.LinkedList
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer

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
  extends LexicaseSelectionMain[S, E, Seq[E]](o) {}

/**
  * This is a base version of the lexicase selection which accepts any fitness extending
  * Seq[E].
  */
class LexicaseSelectionMain[S, E, SeqE <: Seq[E]](o: Ordering[E])(implicit rand: TRandom)
    extends StochasticSelection[S, SeqE](rand) {
  def apply(pop: Seq[(S, SeqE)]): (S, SeqE) = {
    @tailrec def sel(sols: Seq[(S, SeqE)], cases: IndexedSeq[Int]): (S, SeqE) =
      if (sols.size == 1) sols(0)
      else if (cases.isEmpty) sols(rand)
      else {
        val theCase = cases.head
        val ord = new Ordering[(S, SeqE)] {
          override def compare(a: (S, SeqE), b: (S, SeqE)) = o.compare(a._2(theCase), b._2(theCase))
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

/**
  * Faster implementation of Lexicase selection, thanks to:
  *  - assuming that there are only two test outcomes possible: 0 - pass, 1 - failure
  *  - inspecting the list (permutation) of passed tests from the end, as this is more
  *    likely to detect failure early
  */
class LexicaseSelection01[S, E <: Seq[Int]](implicit rand: TRandom)
    extends StochasticSelection[S, E](rand) {
  def apply(pop: Seq[(S, E)]): (S, E) = {
    val n = pop(0)._2.size
    n match {
      case 0 => pop(rand)
      case _ => {
        val t = rand.shuffle(0.until(n).toIndexedSeq).toVector
        // longest series of uninterrupted zeros
        var longest: Int = 0
        var best: List[(S, E)] = Nil
        for ((s, e) <- pop) {
          // start from the end - larger probability of failing a test
          var i: Int = longest - 1
          while (i >= 0 && e(t(i)) == 0)
            i -= 1
          if (i == -1) {
            i = longest
            while (i < n && e(t(i)) == 0)
              i += 1
          }
          if (i > longest) {
            longest = i
            best = (s, e) :: Nil
          } else if (i == longest)
            best = (s, e) :: best
        }
        best.toVector(rand)
      }
    }
  }
}



/**
  * Implementation of lexicase for the regression problems with epsilon automatically
  * computed for each test separately as median absolute deviation. For each test t, a
  * solution passes it when it has error <= best_error + eps_t.
  *
  * This is an implementation of epsilon-lexicase selection as described in [1].
  * For thresholding used is the formula from Equation 5.
  *
  * For details see:
  * [1] William La Cava, Lee Spector, and Kourosh Danai. "Epsilon-Lexicase Selection for Regression."
  * In Proceedings of the Genetic and Evolutionary Computation Conference 2016 (GECCO '16)
  *
  * @param epsForTests epsilon values for subsequent tests. By default computed in the constructor
  *                    of a companion object to be median absolute deviation of each test.
  */
class EpsLexicaseSelection[S, E <: Seq[Double]](epsForTests: Seq[Double])(implicit rand: TRandom)
  extends StochasticSelection[S, E](rand) {

  def apply(pop: Seq[(S, E)]): (S, E) = {
    @tailrec def sel(sols: Seq[(S, E)], cases: IndexedSeq[Int]): (S, E) =
      if (sols.size == 1) sols(0)
      else if (cases.isEmpty) sols(rand)
      else {
        val theCase = cases.head
        val eps = epsForTests(theCase)
        val best = sols.minBy(_._2(theCase))
        sel(sols.filter{ s => s._2(theCase) <= best._2(theCase) + eps}, cases.tail)
      }
    // assumes nonempty pop
    val t = rand.shuffle(pop(0)._2.indices.toIndexedSeq)  // shuffled indices
    val r = sel(pop, t)
    r
  }
}

object EpsLexicaseSelection {
  def apply[S, E <: Seq[Double]](pop: Seq[(S, E)])(implicit rand: TRandom) =
    new EpsLexicaseSelection[S, E](medianAbsDev(pop))

  /**
    * Returns a median absolute deviation for each test. Infinite errors are not taken
    * into account for the median.
    */
  def medianAbsDev[S, E <: Seq[Double]](pop: Seq[(S, E)]): IndexedSeq[Double] = {
    pop.head._2.indices.map{ i =>
      val errs = pop.map(_._2(i)).filter(x => !x.isNaN && !x.isInfinity)
      val medianErr = if (errs.isEmpty) 0.0 else Utils.median(errs)
      val deviations = pop.map { p => math.abs(p._2(i) - medianErr) }
      Utils.median(deviations)
    }.toVector
  }
}



/**
  * EXPERIMENTAL
  *  TODO: E is dummy here; ideally, this should be rewritten without E whatsoever,
  *  but this requires redefinition of Selection trait
  */

class LexicaseSelectionNoEval[S, E](o: Seq[Ordering[S]])(implicit rand: TRandom)
    extends StochasticSelection[S, Seq[E]](rand) {
  def apply(pop: Seq[(S, Seq[E])]) = {
    @tailrec def sel(sols: Seq[(S, Seq[E])], cases: Seq[Ordering[S]]): (S, Seq[E]) =
      if (sols.size == 1) sols(0)
      else if (cases.isEmpty) sols(rand)
      else {
        val ord = cases(rand)
        val best = sols.minBy(_._1)(ord)
        sel(sols.filter(s => ord.compare(s._1, best._1) <= 0), cases.diff(List(ord)))
      }
    // assumes nonempty pop
    sel(pop, o)
  }
}
