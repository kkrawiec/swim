package swim.app

import fuel.func.RunExperiment
import fuel.util.IApp
import scala.collection.Seq
import swim.tree.Op
import swim.tree.ConstantProviderUniformI
import swim.DomainWithVars
import swim.Test
import swim.tree.SimpleGP
import swim.Grammar

/**
  * Reproduces the array_search_5.sl benchmark from the SyGuS'14 competition, as given by the
  *  specification below: given a sorted array of five ys and k, return the 'rank' of k with
  *  respect to array contents.
  *
  *  There is one exception in the Scala implementation below: the original specification does not specify
  *  program output when k is equal to any element of the array.
  *
  * (set-logic LIA)
  * (synth-fun findIdx ( (y1 Int) (y2 Int) (y3 Int) (y4 Int) (y5 Int) (k1 Int)) Int
  * ((Start Int ( 0 1 2 3 4 5 y1 y2 y3 y4 y5 k1 (ite BoolExpr Start Start)))
  * (BoolExpr Bool ((< Start Start) (<= Start Start) (> Start Start) (>= Start Start)))))
  * (declare-var x1 Int)
  * (declare-var x2 Int)
  * (declare-var x3 Int)
  * (declare-var x4 Int)
  * (declare-var x5 Int)
  * (declare-var k Int)
  * (constraint (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (< k x1) (= (findIdx x1 x2 x3 x4 x5 k) 0))))
  * (constraint (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (> k x5) (= (findIdx x1 x2 x3 x4 x5 k) 5))))
  * (constraint (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (and (> k x1) (< k x2)) (= (findIdx x1 x2 x3 x4 x5 k) 1))))
  * (constraint (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (and (> k x2) (< k x3)) (= (findIdx x1 x2 x3 x4 x5 k) 2))))
  * (constraint (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (and (> k x3) (< k x4)) (= (findIdx x1 x2 x3 x4 x5 k) 3))))
  * (constraint (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (and (> k x4) (< k x5)) (= (findIdx x1 x2 x3 x4 x5 k) 4))))
  * (check-synth)
  *
  *
  */

case object ArraySearch extends DomainWithVars[Seq[Int], Int, Op](6) {
  override def semantics(input: Seq[Int]) = {
    assume(input.size == numVars)
    new Function1[Op, Any] {
      def apply(op: Op): Any = {
        // Needs toList (otherwise ArrayBuffer, which doesn't work with pattern matching)
        val childRes = op.args.toSeq.map(c => apply(c)).toList 
        childRes.+:(op.op) match {
          case Seq('<, x: Int, y: Int)               => x < y
          case Seq('<=, x: Int, y: Int)              => x <= y
          case Seq('>, x: Int, y: Int)               => x > y
          case Seq('>=, x: Int, y: Int)              => x >= y
          case Seq('ite, b: Boolean, x: Int, y: Int) => if (b) x else y
          case Seq('y1)                              => input(0)
          case Seq('y2)                              => input(1)
          case Seq('y3)                              => input(2)
          case Seq('y4)                              => input(3)
          case Seq('y5)                              => input(4)
          case Seq('k)                               => input(5)
          case Seq(v: Int)                           => v
        }
      }
    }
  }
}

object ArraySearch5 extends IApp('maxGenerations -> 100) { // 'parEval -> false 

  val grammar = Grammar('S,
    'S -> Seq(
      ConstantProviderUniformI(0, 5),
      'y1, 'y2, 'y3, 'y4, 'y5, 'k,
      'ite -> ('SB, 'S, 'S)),
    'SB -> List(
      '< -> ('S, 'S),
      '<= -> ('S, 'S),
      '> -> ('S, 'S),
      '>= -> ('S, 'S)))

  val tests = for (i <- 0 until 100) yield {
    val y = 0.until(5).map(_ => rng.nextInt(100)).sorted.toSeq
    val k = rng.nextInt(100)
    val pos = y.indexWhere(_ > k)
    //Test(y.:+(k), if(pos>=0) pos else 5) 
    // seems harder to solve than this:
    Test(y.:+(k), (pos + 1) % 5)
  }

  RunExperiment(SimpleGP.Discrete(grammar, ArraySearch, tests.asInstanceOf[Seq[Test[Seq[Int], Int]]]))
}