object Main:
  def main(args: Array[String]): Unit =
    val rand = MyRandom()
    val (board, open, r2) = Game.createGame(6, 6, rand)

    println(Game.boardToString(board, 6, 6))

    val (coord, r3) = Game.randomMove(open, r2)
    println(s"Random move: $coord")