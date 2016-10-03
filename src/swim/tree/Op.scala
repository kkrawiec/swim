package swim.tree

import swim.Program

/*
 * Op is a Program represented as an expression tree (or graph). 
 * Note that the Op class has no type parameters.
 * To enable type-aware manipulation, every Op stores the nonterminal symbol nt that was 
 * used by the underlying grammar to generate that Op. 
 * op represents Op's opcode. 
 */

case class Op(val nt: Any, val op: Any, val args: Op*) extends Program {
  override def hashCode = op.## ^ nt.## ^ args.##
  override def toString = (if (op.isInstanceOf[Symbol]) op.toString.tail else op) +
    (if (args.isEmpty) "" else args.mkString("(", " ", ")"))
  override lazy val size: Int = if (args.isEmpty) 1 else 1 + args.map(_.size).sum
  lazy val hasArguments = args.size > 0
  lazy val isLeaf = args.size == 0
  lazy val height: Int = if (args.isEmpty) 0 else 1 + args.map(_.height).max
  def sizeAndHeight: (Int, Int) = if (args.isEmpty) (1, 0) else {
    val ch = args.map(_.sizeAndHeight)
    (1 + ch.map(_._1).sum, 1 + ch.map(_._2).max)
  }
  def depthInTree(tree: Op, d: Int = 0): Option[Int] = this match {
    case t if t eq tree => Some(d)
    case Op(op: Any, _, a @ _*) => {
      val sub = a.map(c => c.depthInTree(tree, d + 1)).flatten
      if (sub.isEmpty) None else Some(sub.head)
    }
    case _ => None
  }
  def contains(theOp: Any): Boolean =
    if (op == theOp) true
    else args.exists { child => child.contains(theOp) }
  def setArgs(newArgs: Seq[Op]): Op = Op(this.nt, this.op, newArgs:_*)
}

