package swim.tree

import scala.Range

import fuel.Preamble.RndApply
import fuel.util.TRandom

/* Traverses the program tree in depth-first order.
 */
object NodeCollector {
  def apply(tree: Op): List[(Op, Int)] =
    apply(tree, _ => true) ensuring (_.nonEmpty)
  def apply(tree: Op, pred: Op => Boolean): List[(Op, Int)] = {
    var nodes = List[(Op, Int)]()
    def traverse(tree: Op, depth: Int): Unit = {
      if (pred(tree))
        nodes = (tree, depth) :: nodes
      tree.args.foreach(traverse(_, depth + 1))
    }
    traverse(tree, 0)
    nodes
  }
}

/* NodeSelectors are functions that select one node from a tree.  
 * 
 * The simplest node selector: each tree node is equally likely to be selected. 
 * Note: May select root node. 
 */
case class UniformNodeSelector(rng: TRandom) extends (Op => Op) {
  def apply(tree: Op) = {
    val nodes = NodeCollector(tree)
    nodes(rng)._1
  }
}

/* Koza-style node selector. 
 * Uses different probabilities for selecting leaves and internal nodes. 
 */
case class KozaNodeSelector(leafSelectionProb: Double)(implicit rng: TRandom)
    extends (Op => Op) {
  assume(leafSelectionProb >= 0 && leafSelectionProb <= 1.0)
  def apply(tree: Op): Op = {
    val selInternalNode = tree.hasArguments && rng.nextDouble >= leafSelectionProb
    val filter: Op => Boolean = if (selInternalNode) _.hasArguments else !_.hasArguments
    val nodes = NodeCollector(tree, filter)
    nodes(rng)._1
  }
}

/* Pick a depth in the interval [1,height(tree)] and pick a random
 * node at that depth. 
 * Note: Never picks the root node. 
 */
case class UniformDepthNodeSelector(rng: TRandom) extends (Op => Op) {
  def apply(tree: Op): Op = {
    val nodes = NodeCollector(tree)
    if (nodes.size == 1) nodes.head._1
    else {
      val mutationDepth = 1 + rng.nextInt(nodes.maxBy(_._2)._2)
      val selNodes = nodes.filter(_._2 == mutationDepth)
      selNodes(rng)._1
    }
  }
}

/* Replaces an indicated part of program tree with another tree. 
 */
object Replacer {
  // assumes toReplace is a node in tree
  private def apply(replace: Op => Op)(tree: Op, toReplace: Op): Op = {
    // returns the tree and a flag saying if it has been modified w.r.t. original
    def traverse(node: Op): (Op, Boolean) = node match {
      case n if n eq toReplace => (replace(node), true)
      case n if n.isLeaf       => (node, false)
      case _ => {
        val r = Range(0, node.args.size).view
          .map(i => (i, traverse(node.args(i)))).find(_._2._2)
        if (r.isEmpty)
          (node, false)
        else {
          // Can't use copy() method here, as Op's constructor has variable-length 
          // argument list
          (Op(node.nt, node.op, node.args.updated(r.get._1, r.get._2._1): _*), true)
        }
      }
    }
    val r = traverse(tree)
    // TODO: This could be achieved by checking if the root has changed
    // flags are not necessary
    assume(r._2, "toReplace node should be in the tree")
    r._1
  }
  // for subtree mutation
  def apply(tree: Op, toReplace: Op, replacing: Op): Op =
    apply(_ => replacing)(tree, toReplace)
}



