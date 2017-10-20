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
class CodeFactory(val grammar: Grammar, stoppingDepth: Int, maxDepth: Int, isFeasible: Op => Boolean = _ => true)(
    implicit rng: TRandom) {
  val start = grammar.startNT

  def randomProgram: Op = apply(start) // WARNING: Problems with apply()

  class TooLargeException extends Exception
  def apply(typ: Any): Op = try {
    val res = apply(typ, 0)
    if (isFeasible(res)) res
    else apply(typ)
  } catch {
    case _: TooLargeException => apply(typ)
  }

  def apply(typ: Any, depth: Int): Op = {
    if (depth > maxDepth) throw new TooLargeException()
    val prod = if (depth < stoppingDepth) grammar(typ)
      else if (grammar(typ).hasTerminalRHs) grammar.terminalProductions(typ)
      else grammar(typ)
    val rule = prod.right(rng)
    rule match {
      case (op: Any, args: Seq[Any]) => // More than one argument
        Op(typ, op, args.map(apply(_, depth + 1)).toList: _*)
      case (op: Any, args: Product) => // More than one argument
        Op(typ, op, args.productIterator.map(apply(_, depth + 1)).toList: _*)
      case  cp: ConstantProvider =>
        cp(typ)
      case (op: Any, arg: Any) => // One argument (requires special handling because '( )' does not produce Product1
        Op(typ, op, apply(arg, depth + 1))
      case op: Any if grammar.allNT.contains(op) => // Reference to other production (e.g. RealTerminals in Real ::= RealTerminals | RealFunctions)
        apply(op, depth)
      case op: Any => Op(typ, op) // Terminal
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
  def apply(typ: Any) = Op(typ, min + range * rng.nextDouble)
}

case class ConstantProviderUniformI(min: Int, max: Int)(implicit rng: TRandom)
    extends ConstantProvider {
  assume(max >= min)  // allowing all constants being the same 
  val range = max - min + 1
  def apply(typ: Any) = Op(typ, min + rng.nextInt(range))
}

