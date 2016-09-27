package swim.app

import fuel.util.Random
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.Grammar
import swim.tree.ConstantProviderUniformI
import swim.tree.SimpleGP
import swim.tree.CodeFactory
import swim.tree.Op


object TestGPBool extends IApp('benchmark -> "mux6", 'maxGenerations -> 100) {
  RunExperiment(SimpleGP.Discrete(BooleanBenchmark()))
}

object TestGPAlgebra extends IApp('benchmark -> "malcev1", 'maxGenerations -> 100) {
  RunExperiment(SimpleGP.Discrete(AlgebraBenchmark()))
}

object TestGPRegression extends IApp("--benchmark quartic --maxGenerations 100") {
  RunExperiment(SimpleGP.Continuous(RegressionBenchmark()))
}


object TestGPBoolL extends IApp('benchmark -> "mux6", 'maxGenerations -> 30, 'instructions -> "withNeg", 'printResults -> true) {
  RunExperiment(SimpleGP.Lexicase(BooleanBenchmark()))
}

object TestGPBoolISF extends IApp("--benchmark mux6 --maxGenerations 30 --instructions withNeg --printResults true") {
  RunExperiment(SimpleGP.IFS(BooleanBenchmark()))
}
 


object TestGrammar {

  def main(args: Array[String]): Unit = {
    implicit val rnd = new Random

    val rawGrammar = Map[Any, Seq[Any]](
      'S -> Seq(
        ConstantProviderUniformI(-10, 10),
        'x,
        '+ -> ('S, 'S),
        '- -> ('S, 'S),
        '* -> ('S, 'S),
        '/ -> ('S, 'S)))
    println(rawGrammar)
    val g = Grammar(rawGrammar)

    // convenience functions:
    def S(op: Any, args: Op*) = Op('S, op, args: _*)
    def SB(op: Any, args: Op*) = Op('SB, op, args: _*)

    val d = MinDomain
    val input = List(2, 3)
    println(d(Op('S, 7))(input))
    println(d(S('<=, S('x), S('y)))(input))

    val cf = new CodeFactory(g, stoppingDepth = 7, maxDepth = 30)
    println(cf.randomProgram)
    println(cf('SB))

    println("Generating:")
    val progs = for (i <- 0 until 10000) yield cf.randomProgram
    println("Done. Total program size: " + progs.map(_.size).sum)

    println("Executing:")
    progs map { p => d(p)(input) }
    println("Done.")
  }
}

