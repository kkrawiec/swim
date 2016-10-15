package swim.tree

import swim.Program

/**
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



object Op {
  def apply(op: Any, args: Op*): Op = Op('default, op, args:_*)
  
  /**
   * Constructs Op given it's string encoding in the form: Op(ARG1, ARG2, ...).
   * As nonterminal symbol assigned will be 'default.
   * For example from "+(-(a, b), c)" will be created Op('+, Op('-, Op('a), Op('b)), Op('c)).
   * 
   * @param s string encoding of op.
   * @param convertConsts if set to true (default), terminals detected as Boolean, Int, Double or
   * String constants will be converted to instances of those types.
   */
  def fromStr(s: String, convertConsts: Boolean = true): Op = {
    def isBoolean(s: String): Boolean = if (s == "true" || s == "false") true else false
    def isInt(s: String): Boolean = try { val x = s.toInt; true } catch { case _ => false }
    def isDouble(s: String): Boolean = try { val x = s.toDouble; true } catch { case _ => false }
    def isString(s: String): Boolean = if (s.head == '\"' && s.last == '\"') true else false
    def getTerminalOp(s: String): Any = {
      if (convertConsts)
        if (isBoolean(s)) s.toBoolean
        else if (isInt(s)) s.toInt
        else if (isDouble(s)) s.toDouble
        else if (isString(s)) s.substring(1, s.size-1)
        else Symbol(s) // return a symbol, most probably this a variable.
      else
        Symbol(s)
    }
    def getStringOfFirstArg(s: String): (String, Int) = {
      val iComa = s.indexOf(",")
      val iPar = s.indexOf("(")
      if (iComa == -1) // This is a single terminal - return whole string.
        (s, s.size)
      else if (iPar == -1 ||
              (iPar != -1 && iComa < iPar))
        // First argument is a terminal.
        (s.substring(0, iComa), iComa)
      else {
        // First argument is a nonterminal. This is the most problematic case, because
        // we have to reliably get content between it's opening and closing parenthesis.
        var i = iPar + 1
        var parOpened = 1
        while (i < s.size && parOpened > 0) {
          if (s(i) == ')') parOpened -= 1
          else if (s(i) == '(') parOpened += 1
          i += 1
        }
        (s.substring(0, i), i)
      }
    }
    def getRawArgs(s: String): List[String] = {
      val (firstArg, i) = getStringOfFirstArg(s)
      if (i < s.size) firstArg :: getRawArgs(s.substring(i+1))
      else List(firstArg)
    }
    try {
      val i = s.indexOf("(")
      if (i == -1) Op(getTerminalOp(s))  // Returning terminal.
      else {
        val op = s.substring(0, i)
        val sargs = s.substring(i+1, s.size-1)
        val args = getRawArgs(sargs).map{ a => fromStr(a.trim()) }
        Op(Symbol(op), args:_*)
      }
    } catch {
      case _ => throw new Exception("Wrong encoding of Op instance!")
    }
  }
}