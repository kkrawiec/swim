package swim.unittests

import org.junit.Test
import org.junit.Assert._
import swim.tree.Op

final class TestOp {
  @Test def test_Op_fromStr_terminals() {
    assertEquals(Op("asd"), Op.fromStr("asd"))
    assertEquals(Op("5"), Op.fromStr("5"))
  }
  
  @Test def test_Op_fromStr_nonterminals_1() {
    val op1 = Op.fromStr("+(a, b)")
    assertEquals(Op("+", Op("a"), Op("b")), op1)
  }
  
  @Test def test_Op_fromStr_nonterminals_2() {
    val op1 = Op.fromStr("+(-(a),*(b,c))")
    assertEquals(Op("+", Op("-", Op("a")), Op("*", Op("b"), Op("c"))), op1)
  }
  
  @Test def test_Op_fromStr_nonterminals_3() {
    val op1 = Op.fromStr("+(-(*(ab, cd), ef), gh)")
    assertEquals(Op("+", Op("-", Op("*", Op("ab"), Op("cd")), Op("ef")), Op("gh")), op1)
  }
  
  @Test def test_Op_fromStr_nonterminals_4() {
    val op1 = Op.fromStr("+(a, --(b, *(c, d)))")
    assertEquals(Op("+", Op("a"), Op("--", Op("b"), Op("*", Op("c"), Op("d")))), op1)
  }
}