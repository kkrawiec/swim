package swim

import fuel.util.Options
import swim.tree.Op

import scala.collection.Seq

/* Domain defines program semantics. 
 * Programs cannot be executed on their own; it is Domain that executes them. 
 * In other words, Domain serves as a program interpreter. 
 */

trait Domain[I, O, P <: Program] {
  // Note: semantic has Any output type
  def semantics(input: I): Function1[P, Any]

  // The cast of the result to type O results from the assumption 
  // that Domain has output type O  
  def apply(p: P)(input: I): O =
    semantics(input)(p).asInstanceOf[O]
}

abstract class DomainWithVars[I, O, P <: Program](val numVars: Int) extends Domain[I, O, P] {
  assume(numVars > 0)
}

/**
  * Implements a simple domain for recursive functions of signature:
  * I ^ numVars -> O
  * Technically: I ^ numVars -> Option[O]
  * In case the recursion depth (recDepthLimit) is exceeded during execution,
  * interrupts the execution and returns None.
  *
  * This domain handles specially if-then-else (ite) operator so that only a
  * single branch is executed. This makes it possible for the recurrence to stop.
  * RecursiveDomain enforces existence of such an operator and its name can be
  * changed as one wishes (iteSymbol).
  */
abstract class RecursiveDomain[I, O](numVars: Int,
                                     val recDepthLimit: Int,
                                     val recSymbol: Any = 'rec,
                                     val iteSymbol: Any = 'ite)
  extends DomainWithVars[Seq[I], Option[O], Op](numVars) {

  override def semantics(input: Seq[I]): Function1[Op, Option[O]] =
    semanticsWithRecDepthLimit(input, 0)

  /**
    * Provides semantic for each sequence of operator and values of its arguments.
    * To be overridden in your implementation. Result type is Any because operations can
    * return values of different types. This Any can be made to function only internally
    * inside this domain, because value returned by semantics function is converted to O type.
    * This, however, is possible only for single-typed domains or multi-typed domains in
    * which all values inherits from the same trait/class.
    */
  def operationalSemantics(input: Seq[I])(childRes: Seq[Any]): Any

  case object ExceptionTooDeepRecursion extends Exception

  def semanticsWithRecDepthLimit(input: Seq[I], recDepth: Int): Function1[Op, Option[O]] = {
    assume(input.size == numVars, s"Declared number of inputs ($numVars) is different than the provided (${input.size}).")
    new Function1[Op, Option[O]] {

      def operSemantics: Seq[Any] => Any = operationalSemantics(input)

      def apply(program: Op): Option[O] = {
        // Fully recursive interpreter:
        def applyR(op: Op): Any = {
          val a = op.args
          // 'ite and 'rec require special handling. For 'ite some of the arguments
          // are not always evaluated
          if (op.op == iteSymbol)
            if (applyR(a(0)).asInstanceOf[Boolean]) applyR(a(1)) else applyR(a(2))
          else if (op.op == recSymbol) { // recursive call
            if (recDepth + 1 >= recDepthLimit)
              throw ExceptionTooDeepRecursion
            // allow calling a recursive function only with the same
            // number of arguments as the original function
            val argsForRecursiveCall = a.map(x => applyR(x).asInstanceOf[I])
            val recCallResult = semanticsWithRecDepthLimit(argsForRecursiveCall, recDepth + 1)(program)
            if (recCallResult.isDefined) recCallResult.get
            else throw ExceptionTooDeepRecursion
          } else {
            val childRes = op.args.toSeq.map(c => applyR(c)).toList
            operSemantics(childRes.+:(op.op))
          }
        }

        try {
          Some(applyR(program).asInstanceOf[O])
        } catch {
          case ExceptionTooDeepRecursion => None
        }
      }

    }
  }
}



trait ProblemProvider[I, O, P <: Program]
  extends (Options => (Grammar, Domain[I, O, P], Seq[Test[I, O]])) 
