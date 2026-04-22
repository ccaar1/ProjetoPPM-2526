package konane


case class MyRandom(seed: Long):
  def nextInt(n: Int): (Int, MyRandom) =
    val newSeed = (seed * 6364136223846793005L + 1442695040888963407L) & Long.MaxValue
    val value = ((newSeed >>> 16) % n).toInt
    val result = if value < 0 then value + n else value
    (result, MyRandom(newSeed))

object MyRandom:
  def apply(): MyRandom = MyRandom(System.nanoTime())
