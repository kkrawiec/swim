package swim.unittests

import org.junit.Test
import org.junit.Assert._
import swim.tree.Op

final class TestOp {
  @Test def test_Op_fromStr_terminals() {
    assertEquals(Op('true), Op.fromStr("true", convertConsts=false))
    assertEquals(Op("true"), Op.fromStr("true", convertConsts=false, useSymbols=false))
    assertEquals(Op(true), Op.fromStr("true", convertConsts=true))
    assertEquals(Op(Symbol("5")), Op.fromStr("5", convertConsts=false))
    assertEquals(Op("5"), Op.fromStr("5", convertConsts=false, useSymbols=false))
    assertEquals(Op(5), Op.fromStr("5", convertConsts=true))
    assertEquals(Op(Symbol("5.1")), Op.fromStr("5.1", convertConsts=false))
    assertEquals(Op("5.1"), Op.fromStr("5.1", convertConsts=false, useSymbols=false))
    assertEquals(Op(5.1), Op.fromStr("5.1", convertConsts=true))
    assertEquals(Op(Symbol("\"as\"")), Op.fromStr("\"as\"", convertConsts=false))
    assertEquals(Op("\"as\""), Op.fromStr("\"as\"", convertConsts=false, useSymbols=false))
    assertEquals(Op("as"), Op.fromStr("\"as\"", convertConsts=true))
  }
  
  @Test def test_Op_fromStr_nonterminals_1() {
    val op1 = Op.fromStr("+(a b)")
    assertEquals(Op('+, Op('a), Op('b)), op1)
    val op2 = Op.fromStr("+(a b)", useSymbols=false)
    assertEquals(Op("+", Op("a"), Op("b")), op2)
  }
  
  @Test def test_Op_fromStr_nonterminals_2() {
    val op1 = Op.fromStr("+(-(a), *(b, c))", ", ")
    assertEquals(Op('+, Op('-, Op('a)), Op('*, Op('b), Op('c))), op1)
    val op2 = Op.fromStr("+(-(a),*(b,c))", ",")
    assertEquals(Op('+, Op('-, Op('a)), Op('*, Op('b), Op('c))), op2)
    val op3 = Op.fromStr("+(-(a) *(b c))", " ")
    assertEquals(Op('+, Op('-, Op('a)), Op('*, Op('b), Op('c))), op3)
  }
  
  @Test def test_Op_fromStr_nonterminals_3() {
    val op1 = Op.fromStr("+(-(*(ab, cd), ef), gh)", delim = ", ")
    assertEquals(Op('+, Op('-, Op('*, Op('ab), Op('cd)), Op('ef)), Op('gh)), op1)
    val op2 = Op.fromStr("+(-(*(ab, cd), ef), gh)", delim = ", ", useSymbols=false)
    assertEquals(Op("+", Op("-", Op("*", Op("ab"), Op("cd")), Op("ef")), Op("gh")), op2)
  }
  
  @Test def test_Op_fromStr_nonterminals_4() {
    val opT = Op.fromStr("+(a, --(b, *(c, d)))", delim = ", ")
    val opF = Op.fromStr("+(a, --(b, *(c, d)))", delim = ", ")
    assertEquals(Op('+, Op('a), Op('--, Op('b), Op('*, Op('c), Op('d)))), opT)
    assertEquals(opT, opF)
  }

  @Test def test_Op_countAndContains(): Unit = {
    val op = Op.fromStr("recfun(recfun(+(a, recfun(0))))", delim = ", ")
    assertEquals(true, op.contains('recfun))
    assertEquals(3, op.count('recfun))
    assertEquals(true, op.contains('+))
    assertEquals(1, op.count('+))
    assertEquals(false, op.contains('-))
    assertEquals(0, op.count('-))
  }
}