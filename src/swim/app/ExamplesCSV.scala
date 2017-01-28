package swim.app

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import fuel.func.RunExperiment
import fuel.util.IApp
import swim.Grammar
import swim.Tests
import swim.tree.ConstantProviderUniformI
import swim.tree.GPMoves
import swim.tree.Op
import swim.tree.SimpleGP
import fuel.util.Options
import fuel.util.Rng
import fuel.util.CollectorStdout
import fuel.func.BestSoFar
import fuel.core.StatePop
import fuel.util.Collector


object Common {
  def getPassedTestsRatio(bsf: BestSoFar[Op, Int], totalTests: Int)(s: StatePop[(Op, Int)])(implicit coll: Collector) = {
    // Computes ratio of passed tests for a best of run.
    val (op, e) = bsf.bestSoFar.get
    val ratio = if (e <= 0) 1.0 else (totalTests - e).toDouble / totalTests.toDouble
    coll.setResult("best.passedTestsRatio", ratio)
    s
  }
}


object BooleanProblemFromCSV extends App {
  val defaultOpts = Options('maxGenerations -> 20, 'populationSize -> 100, 'csvFile -> "booleanEx1.csv", 'parEval -> false /*multithreaded evaluation off*/)
  implicit val opt = if (this.args.size == 0) defaultOpts else Options(this.args)
  implicit val rng = Rng(opt)
  implicit val coll = CollectorStdout(opt)
  
  val fname = opt.getOption("csvFile").get
  val tests = Tests.fromCSVfile[Boolean](fname, Tests.str2bool)
  //val tests = Tests.fromCSVfile[Boolean](fname, Tests.bin2bool)
  assume(tests.nonEmpty)

  val numVars = tests(0).input.size
  println("Number of input variables: " + numVars)
  
  // instruction set:
  val instrSet = BooleanDomain.instructionSet(opt.paramString("instructions", "default")) +
    (0 -> List(ConstantProviderUniformI(0, numVars - 1))) // input variables
  val grammar = Grammar.fromSingleTypeInstructions(instrSet)

  val domain = BooleanDomain(numVars)
  // fitness function:
  def eval(s: Op) = tests.count(t => domain(s)(t._1) != t._2)

  val gp = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible), eval, SimpleGP.correctDiscrete) {
    override def epilogue = super.epilogue andThen Common.getPassedTestsRatio(bsf, tests.size)
  }
  // launch the GP run
  RunExperiment(gp)
}


object RegressionProblemFromCSV extends App {
  val defaultOpts = Options('maxGenerations -> 20, 'populationSize -> 100, 'csvFile -> "regressionEx1.csv", 'parEval -> false /*multithreaded evaluation off*/)
  implicit val opt = if (this.args.size == 0) defaultOpts else Options(this.args)
  implicit val rng = Rng(opt)
  implicit val coll = CollectorStdout(opt)
  
  val fname = opt.getOption("csvFile").get
  val tests = Tests.fromCSVfile[Double](fname, Tests.str2double)
  assume(tests.nonEmpty)

  val numVars = tests(0).input.size
  println("Number of input variables: " + numVars)
  
  // instruction set:
  val instrSet = FloatingPointDomain.instructionSet(opt.paramString("instructions", "default")) +
    (0 -> List(ConstantProviderUniformI(0, numVars - 1))) // input variables
  val grammar = Grammar.fromSingleTypeInstructions(instrSet)

  val domain = FloatingPointDomain(numVars)
  // fitness function:
  def eval(s: Op) = tests.count(t => domain(s)(t._1) != t._2)

  val gp = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible), eval, SimpleGP.correctDiscrete) {
    override def epilogue = super.epilogue andThen Common.getPassedTestsRatio(bsf, tests.size)
  }
  // launch the GP run
  RunExperiment(gp)
}