package konane

object Main:
  def main(args: Array[String]): Unit =
    val rand = MyRandom()
    val rows = 6
    val cols = 6
    val (board, open, r2) = Game.createGame(rows, cols, rand)

    println(Game.boardToString(board, rows, cols))


    val (f1, t1) = Game.getValidMoves(board, Stone.Black, rows, cols).head match {
      case (f, ts) => (f, ts.head)
    }
    val (b1, o1) = Game.play(board, Stone.Black, f1, t1, open)
    println(s"\n--- Black move-se $f1 -> $t1 ---\n${Game.boardToString(b1.get, rows, cols)}")


    val (f2, t2) = Game.getValidMoves(b1.get, Stone.White, rows, cols).head match {
      case (f, ts) => (f, ts.head)
    }
    val (b2, o2) = Game.play(b1.get, Stone.White, f2, t2, o1)
    println(s"\n--- White move-se $f2 -> $t2 ---\n${Game.boardToString(b2.get, rows, cols)}")
