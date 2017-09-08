package swim.app

import fuel.util.Random
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.Grammar
import swim.tree.ConstantProviderUniformI
import swim.tree.SimpleGP
import swim.tree.CodeFactory
import swim.tree.Op
import fuel.util.OptColl
import swim.Tests
import scala.collection.Seq
import swim.tree.GPMoves
import swim.tree.LexicaseGP
import swim.tree.IFSGP

/* The examples below illustrate a few modes of launching a GP run, 
 * for discrete and continuous domains.  
 * 
 */

/* Applying GP to a toy symbolic regression problem given explicitly
 * as a set of tests:
 */
object TestGPMyProblem extends IApp('maxGenerations -> 20, 'populationSize -> 100) {
  val grammar = Grammar('S, 'S -> Seq(
    0, 1, // input variables
    '+ -> ('S, 'S),
    '- -> ('S, 'S),
    '* -> ('S, 'S),
    '/ -> ('S, 'S)))
  val domain = FloatingPointDomain(2) // 2 input variables
  val tests = Tests(Seq(
    Seq(1.0, 2.0) -> 3.0, // (input0, input1) -> desired output
    Seq(0.0, 0.0) -> 1.0,
    Seq(2.0, 4.0) -> 9.0))
  // Evaluation function (fitness):
  def eval(s: Op) = tests.map(t => math.abs(domain(s)(t._1) - t._2)).sum
  RunExperiment(new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible), 
      eval, SimpleGP.correctContinuous))
}

object TestGPBool extends IApp('benchmark -> "mux6", 'maxGenerations -> 100) {
  RunExperiment(SimpleGP.Discrete(BooleanBenchmark()))
}

/* How to intercept the best synthesized program 
 * 
 */
object TestGPBool2 extends IApp('benchmark -> "mux6", 'maxGenerations -> 100) {
  val d = SimpleGP.Discrete(BooleanBenchmark())
  RunExperiment(d)
  println(f"Best program found: ${d.bsf.bestSoFar}")
}

object TestGPAlgebra extends IApp('benchmark -> "malcev1", 'maxGenerations -> 100) {
  RunExperiment(SimpleGP.Discrete(AlgebraBenchmark()))
}

/* Symbolic regression
 * 
 */
object TestGPRegression extends IApp("--benchmark quartic --maxGenerations 100") {
  RunExperiment(SimpleGP.Continuous(RegressionBenchmark()))
}

/* GP with alternative evaluation/selection methods: 
 * Lexicase and implicit fitness sharing.
 */
object TestGPBoolL extends IApp('benchmark -> "mux6", 'maxGenerations -> 30, 'instructions -> "withNeg", 'printResults -> true) {
  RunExperiment(LexicaseGP(BooleanBenchmark()))
}

object TestGPBoolISF extends IApp("--benchmark mux6 --maxGenerations 30 --instructions withNeg --printResults true") {
  RunExperiment(IFSGP(BooleanBenchmark()))
}

/* Running GP on all benchmarks from a given domain
 * 
 */
object TestGPBoolAll extends App {
  for (b <- BooleanBenchmark.allBenchmarks) {
    new OptColl('benchmark -> b, 'maxGenerations -> 100, 'printResults -> true) {
      RunExperiment(LexicaseGP(BooleanBenchmark()))
    }
  }
}
