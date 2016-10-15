package swim.unittests

import org.junit.Test
import org.junit.Assert._
import swim.tree.Op

final class TestOp {
  @Test def test_Op_fromStr_terminals() {
    assertEquals(Op('true), Op.fromStr("true", convertConsts=false))
    assertEquals(Op(true), Op.fromStr("true", convertConsts=true))
    assertEquals(Op(Symbol("5")), Op.fromStr("5", convertConsts=false))
    assertEquals(Op(5), Op.fromStr("5", convertConsts=true))
    assertEquals(Op(Symbol("5.1")), Op.fromStr("5.1", convertConsts=false))
    assertEquals(Op(5.1), Op.fromStr("5.1", convertConsts=true))
    assertEquals(Op(Symbol("\"as\"")), Op.fromStr("\"as\"", convertConsts=false))
    assertEquals(Op("as"), Op.fromStr("\"as\"", convertConsts=true))
  }
  
  @Test def test_Op_fromStr_nonterminals_1() {
    val op1 = Op.fromStr("+(a, b)")
    assertEquals(Op('+, Op('a), Op('b)), op1)
  }
  
  @Test def test_Op_fromStr_nonterminals_2() {
    val op1 = Op.fromStr("+(-(a),*(b,c))")
    assertEquals(Op('+, Op('-, Op('a)), Op('*, Op('b), Op('c))), op1)
  }
  
  @Test def test_Op_fromStr_nonterminals_3() {
    val op1 = Op.fromStr("+(-(*(ab, cd), ef), gh)")
    assertEquals(Op('+, Op('-, Op('*, Op('ab), Op('cd)), Op('ef)), Op('gh)), op1)
  }
  
  @Test def test_Op_fromStr_nonterminals_4() {
    val opT = Op.fromStr("+(a, --(b, *(c, d)))", convertConsts=true)
    val opF = Op.fromStr("+(a, --(b, *(c, d)))", convertConsts=false)
    assertEquals(Op('+, Op('a), Op('--, Op('b), Op('*, Op('c), Op('d)))), opT)
    assertEquals(opT, opF)
  }
}