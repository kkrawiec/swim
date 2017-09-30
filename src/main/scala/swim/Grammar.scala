package swim

import swim.app.MinDomain
import swim.tree.ConstantProviderUniformI
import swim.tree.Op
import swim.tree.CodeFactory
import fuel.util.Random
import scala.collection.Seq

/* Represents a single grammar production with a nonterminal nt. 
 * The right-hand side (RHS) r is a list of the following:
 * - pairs of (opcode, argList)
 * - atoms (terminals)
 * where argList can be either Seq or Product. 
 * However, the internal representation stored in 'right' is Seq.
 */
case class Production(nt: Any, r: Seq[Any]) {
  assume(r.nonEmpty, "The right-hand of a production cannot be empty")
  val right: Seq[Any] = r.map {
    case (op: Any, args: Seq[Any]) => (op, args)
    case (op: Any, args: Product)  => (op, args.productIterator.toList)
    case a: Any                    => a
  }
  val nonTerminalRHs: Seq[Any] = right.filter(isNonterminalRHS(_))
  val terminalRHs: Seq[Any]    = right.filter(!isNonterminalRHS(_))
  val hasTerminalRHs: Boolean  = terminalRHs.nonEmpty
  def isNonterminalRHS(rhs: Any): Boolean = rhs match {
    case (op: Any, args: Seq[Any]) => true
    case _                         => false
  }

  /**
    * Checks, if the given element is present in the right hand side of this production.
    */
  def contains(s: Any): Boolean = {
    right.exists{
      case (op: Any, args: Seq[Any]) => op == s || args.contains(s)
      case a: Any => a == s
    }
  }
}

/**
  * Represents the syntax of programming language.
  * Abstracts from implementation details of a given language, and from its semantics.
  */
case class Grammar(startNT: Any, g: Map[Any, Seq[Any]]) {
  assume(g.nonEmpty)
  val start = Production(startNT, g(startNT))
  // For fast retrieval of productions, we store them in a map
  // indexed with the nonterminals:
  val allProductions: Map[Any, Production] = g map { case (k, v) => k -> Production(k, v) }
  val allNT: Set[Any] = allProductions.keys.toSet

  private val wrongRHSs = {
    for (p <- allProductions.values) yield {
      p.nonTerminalRHs.filter { rhs =>
        !(rhs match {
          case (op: Any, args: Seq[Any]) =>
            args.forall(
              a => allProductions.keys.exists(_ == a))
          case (op: Any, arg: Any) => allProductions.keys.exists(_ == arg)
          case _                   => throw new InvalidGrammarSyntax(f"Ill-formed right-hand side: $rhs")
        })
      }
    }
  }.flatten
  assume(wrongRHSs.isEmpty,
    f"All nonterminals on the right-hand sides have to have productions starting with them. These don't: ${wrongRHSs}")

  // A subset of productions that have no nonterminals in their RHSs
  val terminalProductions: Map[Any, Production] = allProductions.filter(_._2.hasTerminalRHs)
    .map(p => (p._1 -> Production(p._2.nt, p._2.terminalRHs)))
  def apply(nt: Any): Production = allProductions(nt)

  /**
    * Checks, if the given element is present in the grammar as the right hand side
    * of a certain production.
    */
  def contains(s: Any): Boolean = {
    allProductions.values.exists{ p => p.contains(s) }
  }
}

case object Grammar {
  def apply(startSymbol: Any, g: (Any, Seq[Any])*) = new Grammar(startSymbol,g.toMap )
  def fromMap[N, R](startSymbol: Any,g: Map[N, Seq[R]]) = new Grammar(startSymbol, g map {
    case (k, v) => k.asInstanceOf[Any] -> v.asInstanceOf[Seq[R]]
  })
  // Expects map of entries: (arity -> Seq of instructions)
  def fromSingleTypeInstructions(instr: (Int, Seq[Any])*): Grammar =
    fromSingleTypeInstructions(instr.toMap)
  def fromSingleTypeInstructions(instr: Map[Int, Seq[Any]]): Grammar = {
    val rightHand = instr.flatMap {
      case (arity, instrList) => (arity, instrList) match {
        case (0, instrList)     => instrList
        case (arity, instrList) => instrList.map(instr => instr -> Seq.fill(arity)('S))
      }
    }.toList
    new Grammar('S, Map('S -> rightHand))
  }
}

class InvalidGrammarSyntax(msg: String) extends Exception(msg)

object TestGrammar {
  def main(args: Array[String]): Unit = {
    println(Grammar('S,
      'S -> Seq(
        0, 1, 'x, 'y,
        '+ -> ('S, 'S),
        '- -> ('S, 'S),
        'ite -> ('SB, 'S, 'S)),
      'SB -> List(
        '! -> 'SB,
        '& -> ('SB, 'SB),
        '<= -> ('S, 'S))))
    try {
      Grammar('S,
        'S -> Seq(
          '+ -> ('S, 'S),
          'ite -> ('SB, 'S, 'S)),
        'SB -> List(
          '! -> 'SB,
          '<= -> ('WRONG_NONTERMINAL, 'S)))
    } catch {
      case e: AssertionError => println(f"OK: $e")
    }
    try {
      Grammar('S, 'S -> Seq())
    } catch {
      case e: AssertionError => println(f"OK: $e")
    }

    implicit val rnd = new Random

    val rawGrammar = Map[Any, Seq[Any]](
      'S -> Seq(
        ConstantProviderUniformI(-10, 10),
        'x,
        '+ -> ('S, 'S),
        '- -> ('S, 'S),
        '* -> ('S, 'S),
        '/ -> ('S, 'S)))
    println(rawGrammar)
    val g = Grammar('S, rawGrammar)
    assert(g.contains('S))
    assert(g.contains('+))
    assert(g.contains('x))
    assert(!g.contains('SB))

    // convenience functions:
    def S(op: Any, args: Op*) = Op('S, op, args: _*)
    def SB(op: Any, args: Op*) = Op('SB, op, args: _*)

    val d = MinDomain
    val input = List(2, 3)
    println(d(Op('S, 7))(input))
    println(d(S('<=, S('x), S('y)))(input))

    val cf = new CodeFactory(g, stoppingDepth = 7, maxDepth = 30)
    println(cf.randomProgram)
    println(cf('SB))

    println("Generating:")
    val progs = for (i <- 0 until 10000) yield cf.randomProgram
    println("Done. Total program size: " + progs.map(_.size).sum)

    println("Executing:")
    progs map { p => d(p)(input) }
    println("Done.")
  }
}
