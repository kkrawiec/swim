package swim.app

import scala.collection.Seq

import fuel.func.RunExperiment
import fuel.util.IApp
import swim.Domain
import swim.Grammar
import swim.Test
import swim.tree.Op
import swim.tree.SimpleGP

case object StringDomain extends Domain[String, String, Op] {
  type CharPred = Char => Boolean
  type CharTrans = Char => Char // character transformer
  type Char2Trans = (Char, Char) => Char // character transformer
  override def semantics(input: String) = {
    val l = input.size
    new Function1[Op, Any] {
      def apply(op: Op): Any = {
        
        def isValidIndex(i: Int, s: String) = i >= 0 && i < s.length
        def protectedIndex(i: Int, s: String): Int = {
          val n = s.length
          (i % n + n) % n
        } ensuring { isValidIndex(_,s) }
        def protectedRange(i: Int, j: Int, s: String): (Int,Int) = {
          val pi = protectedIndex(i,s)
          val pj = protectedIndex(j,s)
          (math.min(pi,pj),math.max(pi,pj))
        }
        
        // Needs toList (otherwise ArrayBuffer, which doesn't work with pattern matching)
        val childRes = op.args.toSeq.map(c => apply(c)).toList
        childRes.+:(op.op) match {
          case Seq('head, s: String)                   => if ( s.isEmpty ) "" else s.head.toString
          case Seq('tail, s: String)                   => if ( s.isEmpty ) "" else s.tail
          case Seq('++, s: String, r: String)          => s ++ r
          case Seq('size, s: String)                   => s.size
          case Seq('takeWhile, s: String, p: CharPred) => s.takeWhile { p }
          case Seq('dropWhile, s: String, p: CharPred) => s.dropWhile { p }
          case Seq('substr1, s: String, i: Int) => if( s.isEmpty ) "" else s.substring(protectedIndex(i,s))
          case Seq('substr2, s: String, i: Int, j: Int) => if( s.isEmpty ) "" else { val r = protectedRange( i, j, s ); s.substring(r._1,r._2) }          
          case Seq('isDigit)                           => ((c: Char) => c.isDigit)
          case Seq('isAlpha)                           => ((c: Char) => c.isLetter)
          case Seq('isSpace)                           => ((c: Char) => c.isSpaceChar)
          case Seq('isAlphaOrDigit)                    => ((c: Char) => c.isLetterOrDigit)
          case Seq('isLower)                           => ((c: Char) => c.isLower)
          case Seq('isUpper)                           => ((c: Char) => c.isUpper)
          case Seq('isPunct)                           => ((c: Char) => "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".contains( c ) )
          case Seq('notPred, p: CharPred)              => p.andThen { !_ }
          case Seq('andPred, p: CharPred, r: CharPred) => ((c: Char) => p(c) && r(c))
          case Seq('orPred, p: CharPred, r: CharPred)  => ((c: Char) => p(c) || r(c))
          case Seq('xorPred, p: CharPred, r: CharPred) => ((c: Char) => p(c) != r(c))

          case Seq('map, s: String, t: CharTrans)      => s.map { t }
          case Seq('toLower)                           => ((c: Char) => c.toLower)
          case Seq('toUpper)                           => ((c: Char) => c.toUpper)

          case Seq('reduce, s: String, t: Char2Trans)  => if (s.size < 2) "" else s.reduce { t }.toString
          case Seq('fold, s: String, c: Char, t: Char2Trans) =>
            s.fold(c) { t }.toString
          case Seq('maxChar)           => ((c: Char, d: Char) => Math.max(c, d).toChar)
          case Seq('minChar)           => ((c: Char, d: Char) => Math.min(c, d).toChar)

          case Seq('istr)              => input
          case Seq('ichar, pos: Int)   => if (input.size > 0) input.charAt(protectedIndex(pos,input)).toString else ""
          case Seq(v: String)          => v
          case Seq('+, x: Int, y: Int) => x + y
          case Seq('-, x: Int, y: Int) => x - y
          case Seq('*, x: Int, y: Int) => x * y
          case Seq('/, x: Int, y: Int) => if( y == 0 ) 0 else x / y          
          case Seq(v: Int)             => v
          case Seq(v: Char)            => v
          case _                       => throw new Exception("Invalid instruction: " + op + " or arguments: " + childRes)
        }
      }
    }
  }
}

object StringDomainTest extends IApp('maxGenerations -> 100, 'initMaxTreeDepth -> 20) { // 'parEval -> false 
  
  rng.setSeed(3)
  val grammar = Grammar('str, // initial symbol of the grammar
    'str -> Seq(
      'head -> 'str,
      'tail -> 'str,
      '++ -> ('str, 'str),
      'takeWhile -> ('str, 'charPred),
      'dropWhile -> ('str, 'charPred),
      'map -> ('str, 'charTrans),
      'reduce -> ('str, 'char2Trans),
      'fold -> ('str, 'char, 'char2Trans),
      'ichar -> 'int, // a single character in the input string
      'istr, // entire input string
      "some constant string"),
    'int -> Seq(
      'size -> 'str,
      '+ -> ('int, 'int),
      0, 1, 2 // integer constants
      ),
    'charPred -> Seq(
      'isDigit, 'isAlpha, 'isSpace, 'isAlphaOrDigit, 'isLower, 'isUpper,
      'notPred -> 'charPred,
      'andPred -> ('charPred, 'charPred),
      'orPred -> ('charPred, 'charPred),
      'xorPred -> ('charPred, 'charPred)),
    'char -> Seq(
      ' ', 'a'),
    'charTrans -> Seq(
      'toLower, 'toUpper),
    'char2Trans -> Seq(
      'maxChar, 'minChar))

  print(grammar)

  {
    println("Task 1: prepend the string with its second character")
    val tests = Seq(
      Test("aba", "baba"),
      Test("acka", "cacka"),
      Test("fox", "ofox"))
    RunExperiment(SimpleGP.Discrete(grammar, StringDomain, tests))
  }

  {
    println("Task 2: reverse the input string")
    val tests = Seq(
      Test("", ""),
      Test("a", "a"),
      Test("ab", "ba"),
      Test("abc", "cba"),
      Test("acka", "akca"),
      Test("fox", "xof"))
    RunExperiment(SimpleGP.Discrete(grammar, StringDomain, tests))
  }
  {
    println("Task 3: return substring ending before the first space")
    val tests = Seq(
      Test("", ""),
      Test("a ", "a"),
      Test("ab baa baa", "ab"),
      Test("abc ", "abc"),
      Test("acka macka", "acka"),
      Test(" fox", ""))
    RunExperiment(SimpleGP.Discrete(grammar, StringDomain, tests))
  }
  {
    println("Task 4: toUpper over strings")
    val tests = Seq(
      Test("", ""),
      Test("a ", "A "),
      Test("ab baa baa", "AB BAA BAA"),
      Test("abc ", "ABC "),
      Test("acka macka", "ACKA MACKA"),
      Test(" fox", " FOX"))
    RunExperiment(SimpleGP.Discrete(grammar, StringDomain, tests))
  }
}

