package swim.unittests

import org.junit.Test
import org.junit.Assert._
import swim.tree.Op
import swim.tree.NtOp

final class TestOp {
  @Test def test_NtOp_fromStr_terminals() {
 		assertEquals(NtOp("asd"), NtOp.fromStr("asd"))
 		assertEquals(NtOp("5"), NtOp.fromStr("5"))
  }
  
  @Test def test_NtOp_fromStr_nonterminals_1() {
    val op1 = NtOp.fromStr("+(a, b)")
    assertEquals(NtOp("+", NtOp("a"), NtOp("b")), op1)
  }
  
  @Test def test_NtOp_fromStr_nonterminals_2() {
    val op1 = NtOp.fromStr("+(-(a),*(b,c))")
    assertEquals(NtOp("+", NtOp("-", NtOp("a")), NtOp("*", NtOp("b"), NtOp("c"))), op1)
  }
  
  @Test def test_NtOp_fromStr_nonterminals_3() {
    val op1 = NtOp.fromStr("+(-(*(ab, cd), ef), gh)")
    assertEquals(NtOp("+", NtOp("-", NtOp("*", NtOp("ab"), NtOp("cd")), NtOp("ef")), NtOp("gh")), op1)
  }
  
  @Test def test_NtOp_fromStr_nonterminals_4() {
    val op1 = NtOp.fromStr("+(a, --(b, *(c, d)))")
 		assertEquals(NtOp("+", NtOp("a"), NtOp("--", NtOp("b"), NtOp("*", NtOp("c"), NtOp("d")))), op1)
  }
}