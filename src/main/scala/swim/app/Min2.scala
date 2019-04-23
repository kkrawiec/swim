package swim.app

import fuel.func.RunExperiment
import fuel.util.IApp
import swim.DomainWithVars
import swim.Grammar
import swim.Test
import swim.tree.CodeFactory
import swim.tree.ConstantProviderUniformI
import swim.tree.Op
import swim.tree.SimpleGP

/* Simple example of evolving a program that calculates the minimum of a pair of numbers. 
 * The specification of grammar taken from the max2.sl problem used in the SyGuS contest 
 * (http://www.sygus.org).
 */

/* To enable type-sensitive pattern matching, *all* arguments of an instruction are evaluated
 * in advance. Thus, all tree nodes get evaluated; for instance both the positive and
 * the negative branch of 'ite' get evaluated, no matter what the condition evaluates to. 
 * This could be solved differently; note for instance that 'ite' does not care about 
 * the types of its second and third arguments. 
 */

case object MinDomain extends DomainWithVars[Seq[Int], Int, Op](2) {
  override def semantics(input: Seq[Int]) = {
    assume(input.size == numVars)
    new Function1[Op, Any] {
      def apply(op: Op): Any = {
        val childRes = op.args.map(c => apply(c)) 
        childRes.+:(op.op) match { // or: op.nt :: op.op :: childRes if needed
          case Seq('!, x: Boolean)                   => !x
          case Seq('&, x: Boolean, y: Boolean)       => x & y
          case Seq('+, x: Int, y: Int)               => x + y
          case Seq('-, x: Int, y: Int)               => x - y
          case Seq('<=, x: Int, y: Int)              => x <= y
          case Seq('ite, b: Boolean, x: Int, y: Int) => if (b) x else y
          case Seq('x)                               => input(0)
          case Seq('y)                               => input(1)
          case Seq(v: Any)                           => v
        }
      }
    }
  }
}

object Min2 extends IApp('maxGenerations -> 100) {

  // Grammar is a sequence of productions of the form: Nonterminal -> Seq[RHS] 
  // where RHS can be: 
  // - a terminal (e.g., 'x and 'y below
  // - a pair of a symbol (instruction) and its arguments, each being a nonterminal
  //   like  ('+,('S, 'S))   or   '& -> ('SB, 'SB) below. 
  // Both terminals and nonterminals can be Any. 
  val grammar = Grammar('S,
    'S -> List(
      ConstantProviderUniformI(0, 1),
      'x, 'y,
      ('+, ('S, 'S)),
      ('-, ('S, 'S)),
      ('ite, ('SB, 'S, 'S))),
  'SB -> List(
    '! -> 'SB,
    '& -> ('SB, 'SB),
    '<= -> ('S, 'S)))
  // Rather than using ConstantProvider above, one could explicitly list the terminals 
  // 0, 1, as in the example below (Min2Efficiency). There are no technical reasons to prefer
  // one or another, note however that this choice affects the probability distribution of
  // generating programs. In the grammar above, the constants are generated using only one
  // right-hand side of a production, so they will be less likely to occur in programs than 
  // in the grammar below, where 0 and 1 form two separate right-hand sides. 
  println(grammar)

  val tests = for (i <- 0 until 100) yield {
    val x = rng.nextInt(20)
    val y = rng.nextInt(20)
    Test(Seq(x, y), math.min(x, y))
  }

  RunExperiment(SimpleGP.Discrete(grammar, MinDomain, tests))
}

object Min2Efficiency extends IApp() {
  val grammar = Grammar('S,
    'S -> List(
      0, 1, 'x, 'y,
      '+ -> ('S, 'S),
      '- -> ('S, 'S),
      'ite -> ('SB, 'S, 'S)),
    'SB -> List(
      '! -> 'SB,
      '& -> ('SB, 'SB),
      '<= -> ('S, 'S)))

  val d = MinDomain
  val cf = new CodeFactory(grammar, stoppingDepth = 7, maxDepth = 10)
  println("Generating:")
  val progs = for (i <- 0 until 10000) yield cf.randomProgram
  println("Done. Total program size: " + progs.map(_.size).sum)

  val input = List(2, 3)
  println("Executing:")
  progs map { p => d(p)(input) }
  println("Done.")
}

