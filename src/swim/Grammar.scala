package swim

import swim.tree.ConstantProvider

/* Represents the syntax of programming language. 
 * Abstracts from implementation details of a given language, and from its semantics. 
 * 
 */

case class Grammar(g: Map[Any, Seq[Any]]) {
  assume(g.nonEmpty)
  // Assumes that the first nonterminal is the start symbol of the grammar
  val startNT = g.keys.head
  val start = Production(startNT, g(startNT))
  val allProductions = g map { case (k, v) => (k -> Production(k, v)) }
  // Are all nonterminals in RHS of productions present as LHSs of productions?
  val terminalProductions = allProductions.filter(_._2.hasTerminalRHs)
    .map(p => (p._1 -> Production(p._2.nt, p._2.terminalRHs)))
  println("All:" + allProductions)
  println("Term:" + terminalProductions)
  println("Keys:" + allProductions.keys)
  assume(allProductions.values.forall(
    _.nonTerminalRHs.forall(_ match {
      case (op: Any, args: Product) => //if !rhs.isInstanceOf[ConstantProvider] =>
        args.productIterator.forall(
          a => allProductions.keys.exists(e => { println("1", e, a); e == a }))
      case (op: Any, arg: Any) => allProductions.keys.exists(e => { println("2", e, arg); e == arg })
      case rhs                 => throw new InvalidGrammarSyntax(f"Ill-formed right-hand side: $rhs")
    })))

  def apply(nt: Any) = allProductions(nt)
  println(allProductions)
}

case object Grammar {
  def apply(g: (Any, Seq[Any])*) = new Grammar(g.toMap)
  // Expects map of entries: (arity -> Seq of instructions)
  def fromSingleTypeInstructions(instr: (Int, Seq[Any])*): Grammar =
    fromSingleTypeInstructions(instr.toMap)
  def fromSingleTypeInstructions(instr: Map[Int, Seq[Any]]): Grammar = {
    val rightHand = instr.map({
      case (arity, instrList) => (arity, instrList) match {
        case (0, instrList)     => instrList
        case (arity, instrList) => instrList.map(instr => instr -> Seq.fill(arity)('S))
      }
    }).flatten.toSeq
    new Grammar(Map('S -> rightHand))
  }
}

case class Production(nt: Any, right: Seq[Any]) {
  assume(right.nonEmpty)
  val nonTerminalRHs = right.filter(rhs =>
    rhs.isInstanceOf[Product] && !rhs.isInstanceOf[ConstantProvider])
  val terminalRHs = right.filter(rhs =>
    !rhs.isInstanceOf[Product] || rhs.isInstanceOf[ConstantProvider])
  val hasTerminalRHs = terminalRHs.nonEmpty
  println("AA:" + nonTerminalRHs + " - " + toString)
}

object Production {
  def isNonterminalRHS(rhs:Any) = rhs match {
    case (op: Any, args:Product) => true
    case _ => false
  }
}

class InvalidGrammarSyntax(msg: String) extends Exception(msg)

class TestGrammar {
  def main(args: Array[String]): Unit = {
    val g1 = Grammar(
      'S -> Seq(
        0, 1, 'x, 'y,
        '+ -> ('S, 'S),
        '- -> ('S, 'S),
        'ite -> ('SB, 'S, 'S)),
      'SB -> List(
        '! -> 'SB,
        '& -> ('SB, 'SB),
        '<= -> ('S, 'S)))
    println(g1)
    try{ Grammar(
      'S -> Seq(
        '+ -> ('S, 'S),
        'ite -> ('SB, 'S, 'S)),
      'SB -> List(
        '! -> 'SB,
        '<= -> ('WRONG, 'S)))
    } catch { 
      case e => println(e)
    }
  }
}