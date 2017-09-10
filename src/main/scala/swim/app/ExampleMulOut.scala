package swim.app

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.Seq

import fuel.func.RunExperiment
import fuel.util.IApp
import swim.DomainWithVars
import swim.Grammar
import swim.Tests
import swim.tree.GPMoves
import swim.tree.Op
import swim.tree.SimpleGP

/**
  * This example shows how to implement multiple output regression (a.k.a.
  * multiple target regression). In the use case below, GP searches for
  * a program that maps 2 inputs into 3 outputs. Implementation involves
  * slightly modifying the grammar (compared to conventional symbolic regression)
  * and providing the FloatingPointDomainMulOut domain that can handle such programs.
  * This code is based on TestGPMyProblem in Example.scala
  */

case class FloatingPointDomainMulOut(override val numVars: Int, val numOuts: Int)
    extends DomainWithVars[Seq[Double], Seq[Double], Op](numVars) {
  val fpdomain = FloatingPointDomain(numVars)
  override def semantics(input: Seq[Double]) = {
    val sem = fpdomain.semantics(input)
    assume(input.size == numVars)
    new Function1[Op, Seq[Double]] {
      def apply(op: Op): Seq[Double] = {
        val a = op.args
        assume(a.size == numOuts)
        op.op match {
          case 'root => a.map(sem(_))
        }
      }
    }
  }
}

object TestGPMulOut extends IApp('maxGenerations -> 20, 'populationSize -> 100) {

  val numVars = 2 // number of inputs
  val numOuts = 3 // number of outputs

  val grammar = Grammar('I,
    'I -> Seq(
      'root -> List.fill(numOuts)('S)),
    'S -> Seq(
      0, 1, // input variables
      '+ -> ('S, 'S),
      '- -> ('S, 'S),
      '* -> ('S, 'S),
      '/ -> ('S, 'S)))

  val domain = FloatingPointDomainMulOut(numVars, numOuts)

  // Tests have the form:
  // (input0, input1, ...) -> (desiredOutput0, desiredOutput1, ...)
  val tests = Tests(Seq(
    Seq(1.0, 2.0) -> Seq(3.0, 2.0, -1.0),
    Seq(0.0, 0.0) -> Seq(0.0, 0.0, 0.0),
    Seq(2.0, 4.0) -> Seq(6.0, 8.0, -2.0)))

  def eval(s: Op) = tests.map(t => {
    val outs = domain(s)(t._1)
    // output is a vector, so we have to aggregate the errors:
    outs.zip(t._2).map({ case (out, desOut) => math.abs(out - desOut) }).sum
  }).sum

  RunExperiment(new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, SimpleGP.correctContinuous))
}
