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

  // launch the GP run
  RunExperiment(new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, SimpleGP.correctDiscrete))
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

  // launch the GP run
  RunExperiment(new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, SimpleGP.correctDiscrete))
}