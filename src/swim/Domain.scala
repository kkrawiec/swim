package swim

import fuel.util.Options

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

trait ProblemProvider[I, O, P <: Program]
  extends (Options => (Grammar, Domain[I, O, P], Seq[Test[I, O]])) 
