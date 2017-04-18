package swim.eval

trait Hamming[O] extends Function2[Seq[O], Seq[O], Double] {
  override def apply(x: Seq[O], y: Seq[O]) = {
    assume(x.size == y.size)
    x.zip(y).count(e => e._1 != e._2)
  }
}
object Hamming extends Hamming[Double]

trait SameSign extends Function2[Seq[Double], Seq[Double], Double] {
  override def apply(x: Seq[Double], y: Seq[Double]) = {
    assume(x.size == y.size)
    x.zip(y).count(e => e._1 * e._2 < 0)
  }
}
object SameSign extends SameSign

trait L1 extends Function2[Seq[Double], Seq[Double], Double] {
  override def apply(x: Seq[Double], y: Seq[Double]) = {
    assume(x.size == y.size)
    x.zip(y).map(e => math.abs(e._1 - e._2)).sum
  }
}
object L1 extends L1

trait L2 extends Function2[Seq[Double], Seq[Double], Double] {
  override def apply(x: Seq[Double], y: Seq[Double]) = {
    assume(x.size == y.size)
    math.sqrt(x.zip(y).map(e => (e._1 - e._2) * (e._1 - e._2)).sum)
  }
}
object L2 extends L2

/**
  * Note: avgOut and avgDesOut can be Infinity, which results in correlation NaN
  * Note: Maximized
  *
  * Pearson and other functions are optimized so that when needed, they can be first applied
  * to the first argument only, in which case some values are precomputed.
  * This leads to several-fold speedup.
  *
  */
trait Pearson extends Function2[Seq[Double], Seq[Double], Double] {
  def avgAndStd(x: Seq[Double]) = {
    val mx = x.sum / x.size
    (mx, math.sqrt(x.map(v => (v - mx) * (v - mx)).sum))
  }
  override def apply(x: Seq[Double], y: Seq[Double]) = {
    assume(x.size == y.size)
    val (mx, stdx) = avgAndStd(x)
    val (my, stdy) = avgAndStd(y)
    if (stdx == 0 || stdy == 0) Double.NaN
    else x.zip(y).map(p => (p._1 - mx) * (p._2 - my)).sum / (stdx * stdy)
  }
}
object Pearson extends Pearson

/**
  * Area Under Curve calculated according to Mann-Whitney / Wilcoxon test
  * Maximized.
  * The first argument are the decision labels. They may have arbitrary values,
  * only sign matters
  *
  */
trait AUC extends Function2[Seq[Double], Seq[Double], Double] {
  override def apply(labels: Seq[Double], out: Seq[Double]) = {
    assume(labels.size == out.size)
    val nneg = labels.count(_ <= 0)
    val npos = labels.size - nneg
    assume(nneg * npos != 0, "AUC works for binary problems only")
    assert(out.size == labels.size)
    val ranked = out.zip(labels).sorted(new Ordering[(Double, Double)] {
      def compare(e1: (Double, Double), e2: (Double, Double)) = (e1, e2) match {
        case ((o1, l1), (o2, l2)) if (o1 == o2) => -(l1 compare l2) // ties deteriorate AUC
        case ((o1, _), (o2, _))                 => o1 compare o2
      }
    })
    val sumRanksOfNeg = 1.to(ranked.size).filter(i => ranked(i - 1)._2 < 0).sum
    val auc = 1 + (nneg * (nneg + 1) / 2 - sumRanksOfNeg) / (nneg * npos).toDouble
    assert(auc >= 0 && auc <= 1.0)
    auc
  }
}
object AUC extends AUC

/**
  * Cohen's Kappa
  * Warning: Does not check if out contains only the values from O.
  * If that's not the case, Kappa may be negative
  *  labels should be discrete
  * Note: Maximized.
  */
trait Kappa extends Function2[Seq[Double], Seq[Double], Double] {
  override def apply(labels: Seq[Double], out: Seq[Double]) = {
    assume(labels.size == out.size)
    val n = labels.size.toDouble
    val labelDistribution = labels.groupBy(e => e).map(e => (e._1, e._2.size))
    val outDistribution = out.groupBy(e => e).map(e => (e._1, e._2.size))
    // probability of agreement by chance
    val pchance = labelDistribution.map(kv => kv._2 * outDistribution.getOrElse(kv._1, 0)).sum / (n * n)
    // frequency of actual agreements
    val pactual = out.zip(labels).filter(e => e._1 == e._2).size / n
    //println("pchance: " + pchance + " pactual: " + pactual)
    (pactual - pchance) / (1.0 - pchance)
  }
}
object Kappa extends Kappa

/**
  * Not fully efficient. assumes the negative class is the class with smaller label
  *
  */
trait KappaBinarizing extends Kappa {
  override def apply(labels: Seq[Double], out: Seq[Double]) = {
    val labelDistribution = labels.groupBy(e => e).map(e => (e._1, e._2.size))
    val sortedLabels = labelDistribution.keys.toList.sorted
    val nneg = labelDistribution(sortedLabels(0))
    val negLabel = sortedLabels(0)
    val posLabel = sortedLabels(1)
    assume(labelDistribution.size == 2)
    val sout = out.sorted
    val threshold = (out(nneg - 1) + out(nneg)) / 2
    val outMapped = out.map(v => if (v < threshold) negLabel else posLabel)
    super.apply(labels, outMapped)
  }
}
object KappaBinarizing extends KappaBinarizing


/*
 * object TestKappa extends Tests[Int, Int] with SearchDrivers.Kappa[Int, Int] {
  override def tests = Seq(Test(1, 1), Test(2, 2), Test(3, 3))
  def main(args: Array[String]) {
    println(driver(Seq(1, 2, 3)))
    println(driver(Seq(1, 2, 2)))
    println(driver(Seq(1, 1, 1)))
    // this should not happen in practice: labels that do not occur in the tests 
    println(driver(Seq(3, 4, 5)))
    println(driver(Seq(4, 5, 6)))
  }
}

object TestKappaBin extends Tests[Int, Double] with SearchDrivers.KappaBinarizing[Int] {
  override def tests = Seq(Test(1, 1.0), Test(2, 2.0), Test(3, 2.0))
  def main(args: Array[String]) {
    println(driver(Seq(1.0, 2.0, 3.0)))
    println(driver(Seq(1.0, 2.0, 2.0)))
    println(driver(Seq(1.0, 1.0, 1.0)))
    println(driver(Seq(1.0, 1.0, 2.0)))
    println(driver(Seq(2.0, 1.0, 1.0)))
  }
}
* 
*/
