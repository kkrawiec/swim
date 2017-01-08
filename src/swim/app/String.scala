package swim.app

import scala.collection.immutable.Seq

import fuel.func.RunExperiment
import fuel.util.IApp
import swim.Domain
import swim.Grammar
import swim.Test
import swim.tree.Op
import swim.tree.SimpleGP

case object StringDomain extends Domain[String, String, Op] {
  type CharPred = Char => Boolean
  override def semantics(input: String) = {
    val l = input.size
    new Function1[Op, Any] {
      def apply(op: Op): Any = {
        // Needs toList (otherwise ArrayBuffer, which doesn't work with pattern matching)
        val childRes = op.args.toSeq.map(c => apply(c)).toList
        childRes.+:(op.op) match {
          case Seq('head, s: String)                   => if (s.size > 0) s.substring(0, 1) else ""
          case Seq('tail, s: String)                   => if (s.size > 0) s.substring(1) else ""
          case Seq('++, s: String, r: String)          => s ++ r
          case Seq('size, s: String)                   => s.size
          case Seq('char, pos: Int)                    => if (input.size > 0) input.charAt(pos % l).toString else ""
          case Seq('takeWhile, s: String, p: CharPred) => s.takeWhile { p }
          case Seq('isDigit)                           => ((c: Char) => c.isDigit)
          case Seq('isAlpha)                           => ((c: Char) => c.isLetter)
          case Seq('isSpace)                           => ((c: Char) => c == ' ')
          case Seq('notPred, p: CharPred)              => p.andThen { !_ }
          case Seq('andPred, p: CharPred, r: CharPred) => ((c: Char) => p(c) && r(c))
          case Seq('orPred, p: CharPred, r: CharPred)  => ((c: Char) => p(c) || r(c))
          case Seq('input)                             => input
          case Seq(v: String)                          => v
          case Seq('+, x: Int, y: Int)                 => x + y
          case Seq(v: Int)                             => v
          case _                                       => throw new Exception("Invalid instruction or arguments: " + op.op + " " + childRes)
        }
      }
    }
  }
}

object StringDomainTest extends IApp('maxGenerations -> 100) { // 'parEval -> false 
  // The left-hand side symbol in the first production of the grammar (here: 'str) 
  // becomes the starting symbol: 
  val grammar = Grammar(
    'str -> Seq(
      'head -> 'str,
      'tail -> 'str,
      '++ -> ('str, 'str),
      'takeWhile -> ('str, 'charPred),
      'char -> 'int, // a single character in the input string
      'input, // input string
      "some constant string",
      "another constant string"),
    'int -> Seq(
      'size -> 'str,
      '+ -> ('int, 'int),
      0, 1, 2 // integer constants
      ),
    'charPred -> Seq(
      'isDigit, 'isAlpha, 'isSpace,
      'notPred -> 'charPred,
      'andPred -> ('charPred, 'charPred),
      'orPred -> ('charPred, 'charPred)))

  { // Task 1: prepend the string with its second character
    val tests = Seq(
      Test("aba", "baba"),
      Test("acka", "cacka"),
      Test("fox", "ofox"))
    RunExperiment(SimpleGP.Discrete(grammar, StringDomain, tests))
  }

  { // Task 2: reverse the input string
    val tests = Seq(
      Test("", ""),
      Test("a", "a"),
      Test("ab", "ba"),
      Test("abc", "cba"),
      Test("acka", "akca"),
      Test("fox", "xof"))
    RunExperiment(SimpleGP.Discrete(grammar, StringDomain, tests))
  }
  { // Task 3: return substring ending before the first space
    val tests = Seq(
      Test("", ""),
      Test("a ", "a"),
      Test("ab baa baa", "ab"),
      Test("abc ", "abc"),
      Test("acka macka", "acka"),
      Test(" fox", ""))
    RunExperiment(SimpleGP.Discrete(grammar, StringDomain, tests))
  }}

