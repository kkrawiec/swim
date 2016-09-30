package swim.tree

import fuel.Preamble.RndApply
import fuel.util.TRandom
import swim.Grammar

/**
  * Traverses grammar tree randomly, generating program tree. Once the depth of any node
  * of the program tree exceeds stoppingDepth, starts picking productions that
  * immediately lead to terminals, whenever possible. If the depth exceeds maxDepth,
  * terminates and starts anew.
  *
  * Recommendation:
  *  Even relatively low stoppingDepth values may lead to huge programs. Recommended setting is below 5.
  */
class CodeFactory(val grammar: Grammar, stoppingDepth: Int, maxDepth: Int)(
    implicit rng: TRandom) {
  val start = grammar.startNT

  def randomProgram: Op = apply(start) // WARNING: Problems with apply()

  class TooLargeException extends Exception
  def apply(nt: Any): Op = try {
    apply(nt, 0)
  } catch {
    case _: TooLargeException => apply(nt)
  }

  def apply(nt: Any, depth: Int): Op = {
    if (depth > maxDepth) throw new TooLargeException()
    val prod = if (depth < stoppingDepth) grammar(nt)
    else if (grammar(nt).hasTerminalRHs) grammar.terminalProductions(nt)
    else grammar(nt)
    val rule = prod.right(rng)
    rule match {
      case (op: Any, args: Seq[Any]) => // More than one argument
        Op(nt, op, args.map(apply(_, depth + 1)).toList: _*)
      case (op: Any, args: Product) => // More than one argument
        Op(nt, op, args.productIterator.map(apply(_, depth + 1)).toList: _*)
      case  cp: ConstantProvider =>
        cp(nt)
      case (op: Any, arg: Any) => // One argument (requires special handling because '( )' does not produce Product1
        Op(nt, op, apply(arg, depth + 1))
      case op: Any => Op(nt, op) // Terminal
    }
  }
}


/* Constant providers are objects that can generate constant values (terminals) 
 * on demand, typically at random. They should be functions Any => Op
 */
trait ConstantProvider extends (Any => Op)

case class ConstantProviderUniformD(min: Double, max: Double)(implicit rng: TRandom)
    extends ConstantProvider {
  assume(max > min)
  val range = max - min
  def apply(nt: Any) = Op(nt, min + range * rng.nextDouble)
}

case class ConstantProviderUniformI(min: Int, max: Int)(implicit rng: TRandom)
    extends ConstantProvider {
  assume(max >= min)  // allowing all constants being the same 
  val range = max - min + 1
  def apply(nt: Any) = Op(nt, min + rng.nextInt(range))
}

