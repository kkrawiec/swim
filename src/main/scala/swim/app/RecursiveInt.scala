package swim.app

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.Seq
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.{DomainWithVars, Grammar, RecursiveDomain, Tests}
import swim.tree.GPMoves
import swim.tree.Op
import swim.tree.SimpleGP

/**
  * Implements simple domain for recursive functions of signature:
  * Int^numVars -> Int
  * Technically: Long^numVars -> Option[Long]
  * In case the recursion depth (recDepthLimit) is exceeded during execution,
  * interrupts the execution and returns None
  */
case class RecursiveIntDomain(override val numVars: Int,
                              val recDepthLimit: Int)
    extends DomainWithVars[Seq[Long], Option[Long], Op](numVars) {

  override def semantics(input: Seq[Long]): Function1[Op, Option[Long]] =
    semanticsWithRecDepthLimit(input, 0)

  case object ExceptionTooDeepRecursion extends Exception

  def semanticsWithRecDepthLimit(input: Seq[Long], recDepth: Int): Function1[Op, Option[Long]] = {
    assume(input.size == numVars)
    new Function1[Op, Option[Long]] {

      def apply(program: Op): Option[Long] = {

        // Fully recursive interpreter:
        def applyR(op: Op): Any = {
          val a = op.args
          // 'ite and 'rec require special handling as some of their
          // arguments are not always evaluated
          if (op.op == 'ite)
            if (applyR(a(0)).asInstanceOf[Boolean]) applyR(a(1)) else applyR(a(2))
          else if (op.op == 'rec) { // recursive call
            if (recDepth + 1 >= recDepthLimit)
              throw ExceptionTooDeepRecursion
            // allow calling a recursive function only with the same 
            // number of arguments as the original function
            assume(a.size == numVars)
            val argsForRecursiveCall = a.map(x => applyR(x).asInstanceOf[Long])
            val recCallResult = semanticsWithRecDepthLimit(argsForRecursiveCall, recDepth + 1)(program)
            if (recCallResult.isDefined) recCallResult.get
            else throw ExceptionTooDeepRecursion
          } else {
            val childRes = op.args.toSeq.map(c => applyR(c)).toList
            childRes.+:(op.op) match {
              case Seq('+, a: Long, b: Long)         => a + b
              case Seq('-, a: Long, b: Long)         => a - b
              case Seq('*, a: Long, b: Long)         => a * b
              case Seq('/, a: Long, b: Long)         => if (b != 0) a / b else 1L
              case Seq('%, a: Long, b: Long)         => if (b != 0) a % b else 1L

              case Seq('<, a: Long, b: Long)         => a < b
              case Seq('>, a: Long, b: Long)         => a > b
              case Seq('==, a: Long, b: Long)        => a == b

              case Seq('and, a: Boolean, b: Boolean) => a && b
              case Seq('or, a: Boolean, b: Boolean)  => a || b
              case Seq('not, a: Boolean)             => !a

              case Seq('a, a: Int)                   => input(a)
              case Seq(b: Boolean)                   => b
              case Seq(v: Long)                      => v
              case Seq(v: Int)                       => v
            }
          }
        }

        try {
          Some(applyR(program).asInstanceOf[Long])
        } catch {
          case ExceptionTooDeepRecursion => None
        }
      }

    }
  }
}


// This time inheriting from the universal template defined in SWIM
case class RecursiveIntDomain2(override val numVars: Int, override val recDepthLimit: Int, recursionSymbol: Symbol = 'rec)
  extends RecursiveDomain[Long, Long](numVars, recDepthLimit, recursionSymbol) {
  override def operationalSemantics(input: Seq[Long])(childRes: Seq[Any]): Any = {
    childRes match {
      case Seq('+, a: Long, b: Long)         => a + b
      case Seq('-, a: Long, b: Long)         => a - b
      case Seq('*, a: Long, b: Long)         => a * b
      case Seq('/, a: Long, b: Long)         => if (b != 0) a / b else 1L
      case Seq('%, a: Long, b: Long)         => if (b != 0) a % b else 1L

      case Seq('<, a: Long, b: Long)         => a < b
      case Seq('>, a: Long, b: Long)         => a > b
      case Seq('==, a: Long, b: Long)        => a == b

      case Seq('and, a: Boolean, b: Boolean) => a && b
      case Seq('or, a: Boolean, b: Boolean)  => a || b
      case Seq('not, a: Boolean)             => !a

      case Seq('a, a: Int)                   => input(a)
      case Seq(b: Boolean)                   => b
      case Seq(v: Long)                      => v
      case Seq(v: Int)                       => v
    }
  }
}



case object TestRecursiveDomain
    extends IApp('maxGenerations -> 100, 'populationSize -> 1000, 'parEval -> true) {

  val nArgs = 1 // the number of input variables
  val grammar = Grammar('I,
    'I -> Seq(
      '+ -> ('I, 'I),
      '- -> ('I, 'I),
      '* -> ('I, 'I),
      //'/ -> ('I, 'I),
      //'% -> ('I, 'I),
      'ite -> ('B, 'I, 'I),
      'a -> ('A), // argument (input)
      'rec -> ('I), // recursive call
      0L, 1L//, // int constants
      ),
    'B -> Seq(
      //true, false,
      //'not -> ('B),
      //'and -> ('B, 'B),
      //'or -> ('B, 'B),
      '< -> ('I, 'I),
      '> -> ('I, 'I),
      '== -> ('I, 'I)),
    'A -> 0.until(nArgs)) // argument index

  val numExamples = 12
  // 0-based indexing of series elements
  def factorial(n: Long): Long = if (n == 0) 1 else n * factorial(n - 1)
  def fibonacci(n: Long): Long = if (n <= 1) 1 else fibonacci(n - 1) + fibonacci(n - 2)
  val targetFunction = factorial _ // Target function: factorial or Fibonacci
  val tests = Tests(0.until(numExamples).map(n => (Seq(n.toLong), targetFunction(n))))

  val domain = RecursiveIntDomain(nArgs, numExamples + 1 /* recursion depth limit */ )

  // Evaluation function (fitness):
  val worstPossibleFitness = Long.MaxValue
  def errAbsolute(a: Long, b: Long) = math.abs(a - b)
  def errHamming(a: Long, b: Long) = if (a == b) 0 else 1
  val err = errHamming _

  val penalizeForLackOfRecursiveCalls = true
  def eval(s: Op) = {
    if (penalizeForLackOfRecursiveCalls && !s.contains('rec)) worstPossibleFitness
    else {
      val execContext = domain(s) _ // TODO: Introduce this in other Swim Examples
      val outputs = tests.toStream.map(test => execContext(test.input)).takeWhile(_.isDefined)
      if (outputs.size < tests.size) worstPossibleFitness
      else tests.zip(outputs).map { case (test, actualOut) => err(actualOut.get, test.output).toLong } sum
    }
  }

  def correctDiscrete = (_: Any, e: Long) => e == 0

  val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, correctDiscrete)
  RunExperiment(alg)

  // For successful runs, show how/whether the best-of-run generalizes
  val bsf = alg.bsf.bestSoFar.get
  if (bsf._2 == 0) {
    val prog = bsf._1
    val maxTest = 20
    val domainTest = RecursiveIntDomain(nArgs, maxTest)
    for (n <- 0.until(maxTest))
      println(n + " : " + domainTest(prog)(Seq(n)))
  }
}
