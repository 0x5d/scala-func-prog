package co.castillobgr.collection

import co.castillobgr.Option
import co.castillobgr.Some
import co.castillobgr.None

object Seq {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}
