package co.castillobgr

object Main {
  def main(args: Array[String]): Unit = {
    val rng = util.random.SimpleRNG(2)
    println(util.random.SimpleRNG.ints(10)(rng))
  }
}
