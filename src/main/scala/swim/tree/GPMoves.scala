package swim.tree

import fuel.util.Options
import fuel.util.TRandom
import fuel.moves.Moves
import fuel.func.SearchOperator
import fuel.Preamble.RndApply
import swim.Grammar
import scala.collection.Seq
import scala.annotation.tailrec

/**
  * GPMoves provides the default set of GP operators.
  * Single-node replacer not implemented yet.
  *
  */
case class GPMoves(grammar: Grammar, isFeasible: Op => Boolean = (_: Op) => true)(
  implicit rng: TRandom, opt: Options)
    extends Moves[Op] {
  val initMaxTreeDepth = opt('initMaxTreeDepth, 5, (_: Int) >= 0)
  val stoppingDepthRatio = opt('stoppingDepthRatio, 0.8, ((x: Double) => x >= 0 && x <= 1.0))
  val cfPrograms = new CodeFactory(grammar, math.round(stoppingDepthRatio * initMaxTreeDepth).toInt,
    initMaxTreeDepth, isFeasible)
  val maxSubtreeDepth = opt('maxSubtreeDepth, 5, (_: Int) > 0)
  val cfFragments = new CodeFactory(grammar, math.round(stoppingDepthRatio * maxSubtreeDepth).toInt,
    maxSubtreeDepth, isFeasible)
  def ns = UniformNodeSelector(rng) // or: KozaNodeSelector(0.1)

  @tailrec
  private def newFeasibleSolution: Op = {
    val p = cfPrograms.randomProgram
    if (isFeasible(p)) p
    else newFeasibleSolution
  }
  override def newSolution = newFeasibleSolution

  def subtreeMutation = (p: Op) => {
    val toReplace = ns(p)
    val replacing = cfFragments(toReplace.nt)
    Replacer(p, toReplace, replacing)
  }

  def treeSwappingCrossover = (p1: Op, p2: Op) => {
    var nodes = List[(Op, Int)]()
    var toSwap1: Op = null
    // This root is guaranteed to terminate, as both programs share the same nonterminal
    // (nt) at the tree root node, i.e., the starting nonterminal of the grammar. 
    do {
      toSwap1 = ns(p1)
      val filter = (op: Op) => op.nt == toSwap1.nt
      nodes = NodeCollector(p2, filter)
    } while (nodes.isEmpty)
    val toSwap2 = nodes(rng)._1
    (Replacer(p1, toSwap1, toSwap2), Replacer(p2, toSwap2, toSwap1))
  }

  override def moves = Seq(
    SearchOperator(subtreeMutation, isFeasible),
    SearchOperator(treeSwappingCrossover, isFeasible))
}

object GPMoves {
  def apply(grammar: Grammar)(implicit rng: TRandom, opt: Options) =
    new GPMoves(grammar)
}

