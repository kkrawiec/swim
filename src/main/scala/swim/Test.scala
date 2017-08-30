package swim

import scala.collection.Seq

import fuel.util.CodeExecutor
import scala.io.Source

/* A test represents a pair of program input and the corresponding desired output.
 * 
 */
class Test[I, O](val input: I, val output: O) {
  val _1: I = input
  val _2: O = output
  override def toString: String = s"Test($input, $output)"
}
object Test {
  def apply[I, O](input: I, output: O) = new Test(input, output)
  implicit def fromPair[I, O](input: I, output: O) = new Test(input, output)
  implicit def fromTuple[I, O](t: Tuple2[I, O]) = new Test(t._1, t._2)
}

/* Helper methods for generating tests from snippets of Scala code via reflection.
 */
object Tests {
  def apply[I, O](tests: Seq[(I, O)]) = tests.map(t => Test[I, O](t._1, t._2)).toIndexedSeq
  def apply[I, O](tests: Seq[(Int, Int)], numVars: Int) =
    tests.map(t => Test[Seq[Boolean], Boolean](intToBoolSeq(t._1, numVars), intToBoolSeq(t._2, 1).head)).toIndexedSeq

  def apply[I, O](testGenerator: String, numVars: Int) = {
    val l = CodeExecutor(testGenerator).asInstanceOf[Seq[Tuple2[_, _]]]
    assert(l.nonEmpty)
    // For Boolean problems:
    if (l.forall({
      case (i, o) => i.isInstanceOf[Int] && o.isInstanceOf[Int] &&
        (o == 0 || o == 1)
    })) {
      assume(numVars < 32, "Too many inputs")
      CodeExecutor(testGenerator).asInstanceOf[Seq[(Int, Int)]]
        .map(t => Test(intToBoolSeq(t._1, numVars), intToBoolSeq(t._2, 1).head).asInstanceOf[Test[I, O]])
    } else
      CodeExecutor(testGenerator).asInstanceOf[Seq[Tuple2[I, O]]].map(t => Test(t._1, t._2)).toIndexedSeq
  }

  private def intToBoolSeq(i: Int, nBits: Int): Seq[Boolean] = {
    val s = i.toBinaryString
    assert(nBits >= s.size, "Not enough bits: i: " + i + " nBits: " + nBits)
    (List.fill(nBits - s.size)(false) ++ s.map(_ == '1')).toIndexedSeq
  }
  private def b2s(b: Boolean) = if (b) 1 else 0

  def toT[T](s: String) = try {
    s.asInstanceOf[T]
  } catch {
    case _: Throwable => throw new Exception(f"Cannot convert value $s to target type")
  }

  def str2bool(s: String) = s match {
    case "true"  => java.lang.Boolean.TRUE
    case "false" => java.lang.Boolean.FALSE
  }
  def bin2bool(s: String) = s match {
    case "1" => java.lang.Boolean.FALSE
    case "0" => java.lang.Boolean.TRUE
  }
  def str2double(s: String) = s.toDouble
  def str2int(s: String) = s.toInt

  def fromCSVfile[T](fname: String, conv: Function1[String, T] = toT _) = {
    val src = Source.fromFile(fname)
    val lines = src.getLines().map(_.split(",|;|\t|\\s+")).toList
    if (lines.map(_.size).toSet.size != 1)
      throw new Exception("Malformed CSV: Varying number of values per line")
    val res = lines.map(l => Test(l.dropRight(1).toList.map(conv(_)), conv(l.last))).toIndexedSeq
    src.close()
    res
  }
}

object TestTest {
  def main(args: Array[String]): Unit = {
    println(Test(Seq(3, 1), 8) == Test(Seq(3, 1), 8))
    println(Test(Seq(3, 1), None) == Test(Seq(3, 1), None))
    println(Test(Seq(3, 1), None) == Test(Seq(3, 1), Some(1)))
  }
}