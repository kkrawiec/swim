package swim.tree

import scala.Ordering
import fuel.core.StatePop
import fuel.func._
import fuel.moves.Moves
import fuel.util.Collector
import fuel.util.Options
import fuel.util.TRandom
import swim.ProblemProvider
import swim.eval.{IFSEval, LexicaseSelection, LexicaseSelectionMain}
import swim.Test
import swim.Domain
import swim.Grammar

import scala.collection.Seq

/*
  * A class for realizing the workflow of the conventional 
  * tree-based GP (multi-type or single-type).
  * 
  */
class SimpleGP[E](moves: Moves[Op],
                  eval: Op => E,
                  stop: (Op, E) => Boolean = (s: Op, e: E) => false)
                 (implicit opt: Options, coll: Collector, rng: TRandom,
                  ordering: Ordering[E])
    extends SimpleEA[Op, E](moves, eval, stop)(opt, coll, rng, ordering) {

  override def iter = SimpleBreeder(selection, moves: _*) andThen evaluate

  val checkSuccess = (s: StatePop[(Op, E)]) => {
    val cor = s.find(e => stop(e._1, e._2))
    coll.setResult("successRate", if (cor.isDefined) 1.0 else 0.0)
    coll.setResult("lastGeneration", it.count)
    s
  }
  override def algorithm = super.algorithm andThen checkSuccess
}

/* Convenience methods for creating instances of SimpleGP tailored
 * to different domains and classes of problems (e.g., discrete and continuous). 
 */
object SimpleGP {

  def Discrete[I, O](pprov: ProblemProvider[I, O, Op])(
    implicit opt: Options, coll: Collector, rng: TRandom): SimpleGP[Int] = {
    implicit val (grammar, domain, tests) = pprov(opt)
    Discrete(grammar, domain, tests)
  }
  def Discrete[I, O](grammar: Grammar, domain: Domain[I, O, Op], tests: Seq[Test[I, O]])(
    implicit opt: Options, coll: Collector, rng: TRandom): SimpleGP[Int] = {
    def eval(s: Op) = tests.count(t => domain(s)(t._1) != t._2)
    new SimpleGP(GPMoves(grammar, defaultFeasible), eval, correctDiscrete)
  }
  def correctDiscrete = (_: Any, e: Int) => e == 0

  def Continuous(pprov: ProblemProvider[Seq[Double], Double, Op])(
    implicit opt: Options, coll: Collector, rng: TRandom) = {
    implicit val (grammar, domain, tests) = pprov(opt)
    def eval(s: Op) = tests.map(t => math.abs(domain(s)(t._1) - t._2)).sum
    new SimpleGP(GPMoves(grammar, defaultFeasible), eval, correctContinuous)
  }
  def correctContinuous = (_: Any, e: Double) => e < 10e-9

  def defaultFeasible(implicit opt: Options) = {
    val maxTreeDepth = opt('maxTreeDepth, 12, (_: Int) > 0)
    (p: Op) => p.height < maxTreeDepth
  }
}

/* Implicit fitness sharing
 * 
 */
object IFSGP {
  def apply[I, O](pprov: ProblemProvider[I, O, Op])(
    implicit opt: Options, coll: Collector, rng: TRandom) = {
    implicit val (grammar, domain, tests) = pprov(opt)
    val moves = GPMoves(grammar, SimpleGP.defaultFeasible)
    new EACore[Op, IFSEval.Fitness](moves, IFSEval(tests, domain), correct) {
      val selection = TournamentSelection[Op, IFSEval.Fitness](new IFSEval.Ord)
      override def iter = SimpleBreeder(selection, moves: _*) andThen evaluate
    }
  }
  def correct[I, O] = (_: Op, e: IFSEval.Fitness) => e.numFailedTests == 0
}

/* Lexicase selection GP
 * 
 */
object LexicaseGP {
  def apply[I, O](pprov: ProblemProvider[I, O, Op])
                 (implicit opt: Options, coll: Collector, rng: TRandom): LexicaseGP = {
    implicit val (grammar, domain, tests) = pprov(opt)
    def eval(s: Op) = tests.map(t => if (domain(s)(t._1) == t._2) 0 else 1)
    val moves = GPMoves(grammar, SimpleGP.defaultFeasible)
    new LexicaseGP(moves, eval)
  }
  def correct = (_: Any, e: Seq[Int]) => e.forall(_ == 0)
}



class LexicaseGP(moves: Moves[Op], eval: Op => Seq[Int],
                 correct: (Op, Seq[Int]) => Boolean = LexicaseGP.correct)
                (implicit opt: Options, coll: Collector, rng: TRandom)
    extends LexicaseGPMain[Int, Seq[Int]](moves, eval, correct, MaxPassedOrdering)


class LexicaseGPMain[E, SeqE <: Seq[E]](moves: Moves[Op],
                                        eval: Op => SeqE,
                                        correct: (Op, SeqE) => Boolean,
                                        orderingBsf: Ordering[SeqE])
                (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends EACore[Op, SeqE](moves,
    if (opt('parEval, true)) ParallelEval(eval) else SequentialEval(eval),
    correct) {

  val selection: StochasticSelection[Op, SeqE] = new LexicaseSelectionMain[Op, E, SeqE](ordering)
  override def iter = SimpleBreeder(selection, moves: _*) andThen evaluate

  val bsf = BestSoFar[Op, SeqE](orderingBsf, it)
  override def report: Function1[StatePop[(Op, SeqE)], StatePop[(Op, SeqE)]] = bsf

  val checkSuccess = (s: StatePop[(Op, SeqE)]) => {
    val cor = s.find(e => correct(e._1, e._2))
    coll.setResult("successRate", if (cor.isDefined) 1.0 else 0.0)
    coll.setResult("lastGeneration", it.count)
    if (cor.isDefined)
      coll.setResult("correctProgram", cor.get)
    s
  }
  override def algorithm = super.algorithm andThen checkSuccess
}


// Assumptions: 0 - correct on test, 1 - incorrect.
object MaxPassedOrdering extends Ordering[Seq[Int]] {
  def compare(a:Seq[Int], b:Seq[Int]) = a.sum compare b.sum
}
// Assumptions: 0 - correct on test, 1 - incorrect.
object LongerOrMaxPassedOrdering extends Ordering[Seq[Int]] {
  def compare(a:Seq[Int], b:Seq[Int]) = if (a.size == b.size) a.sum compare b.sum
                                        else b.size compare a.size // longer sequences are preffered
}

