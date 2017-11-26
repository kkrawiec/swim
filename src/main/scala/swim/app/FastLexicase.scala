package swim.app

import fuel.func.EACore
import fuel.func.RunExperiment
import fuel.func.SimpleBreeder
import fuel.util.Collector
import fuel.util.IApp
import fuel.util.Options
import fuel.util.TRandom
import swim.ProblemProvider
import swim.eval.LexicaseSelection01
import swim.tree.GPMoves
import swim.tree.LexicaseGP
import swim.tree.Op
import swim.tree.SimpleGP

object LexicaseGP01 {
  def apply[I, O](pprov: ProblemProvider[I, O, Op])(
    implicit opt: Options, coll: Collector, rng: TRandom): EACore[Op, Seq[Int]] = {
    implicit val (grammar, domain, tests) = pprov(opt)
    def eval(s: Op) = tests.map(t => if (domain(s)(t._1) == t._2) 0 else 1)
    val moves = GPMoves(grammar, SimpleGP.defaultFeasible)
    new LexicaseGP(moves, eval) {
      val sel = new LexicaseSelection01[Op, Seq[Int]]()
      override def iter = SimpleBreeder(sel, moves: _*) andThen evaluate
    }
  }
  def correct = (_: Any, e: Seq[Int]) => e.forall(_ == 0)
}

object ExperimentalTestGPBoolL01 extends IApp('benchmark -> "mux6", 'maxGenerations -> 50, 'instructions -> "withNeg", 'printResults -> true) {
  RunExperiment(LexicaseGP01(BooleanBenchmark()))
}

object ExperimentalTestGPBoolL extends IApp('benchmark -> "mux6", 'maxGenerations -> 50, 'instructions -> "withNeg", 'printResults -> true) {
  RunExperiment(LexicaseGP(BooleanBenchmark()))
}