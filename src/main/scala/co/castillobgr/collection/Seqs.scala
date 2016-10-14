package co.castillobgr.collection

import co.castillobgr.Option
import co.castillobgr.None
import co.castillobgr.Some

object Seqs {

  def mean(ns: Seq[Double]): Option[Double] =
    if (ns.isEmpty) None
    else Some(ns.sum / ns.length)

  def variance(ns: Seq[Double]): Option[Double] =
    mean(ns).flatMap(m => mean(ns.map(x => math.pow(x - m, 2))))
}
