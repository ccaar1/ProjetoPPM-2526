package konane

import scala.collection.parallel.immutable.ParMap
import scala.annotation.tailrec

object Game:

  // Four orthogonal directions: (dRow, dCol)
  val directions: List[(Int, Int)] = List((-1, 0), (1, 0), (0, -1), (0, 1))

  // Infer board dimensions from a non-empty board
  private def inferDimensions(board: Board): (Int, Int) =
    if board.isEmpty then (0, 0)
    else
      val keys = board.seq.keys
      (keys.map(_._1).max + 1, keys.map(_._2).max + 1)

 //inicializar board

  def initBoard(rows: Int, cols: Int): Board =
    val entries = for
      r <- (0 until rows).toList
      c <- (0 until cols).toList
      stone = if (r + c) % 2 == 0 then Stone.Black else Stone.White
    yield (r, c) -> stone
    ParMap(entries*)

  def removeInitialStones(board: Board, rows: Int, cols: Int, rand: MyRandom): (Board, List[Coord2D], MyRandom) =
    val corners = List((0, 0), (0, cols - 1), (rows - 1, 0), (rows - 1, cols - 1))
    val centerR = rows / 2
    val centerC = cols / 2
    val centers = if rows % 2 == 0 && cols % 2 == 0 then
      List((centerR - 1, centerC - 1), (centerR - 1, centerC), (centerR, centerC - 1), (centerR, centerC))
    else List((centerR, centerC))

    val candidates = corners ++ centers

    val validPairs: List[(Coord2D, Coord2D)] = candidates.flatMap { pos =>
      board.get(pos) match
        case Some(stone) =>
          directions.flatMap { case (dr, dc) =>
            val adj = (pos._1 + dr, pos._2 + dc)
            board.get(adj) match
              case Some(adjStone) if adjStone != stone =>
                if stone == Stone.Black then List((pos, adj))
                else List((adj, pos))
              case _ => Nil
          }
        case None => Nil
    }.distinct

    val (idx, newRand) = rand.nextInt(validPairs.size.max(1))
    val (blackPos, whitePos) = if validPairs.nonEmpty then validPairs(idx) else
      ((0, 0), (0, 1))

    val newBoard = board - blackPos - whitePos
    val openCoords = List(blackPos, whitePos)
    (newBoard, openCoords, newRand)

  def createGame(rows: Int, cols: Int, rand: MyRandom): (Board, List[Coord2D], MyRandom) =
    val board = initBoard(rows, cols)
    removeInitialStones(board, rows, cols, rand)

 //T1 random move - coordernada aleatoria da lista de posicoes livres

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) =
    lstOpenCoords match
      case Nil => throw new IllegalArgumentException("Empty list")
      case single :: Nil => (single, rand)
      case _ =>
        val (idx, newRand) = rand.nextInt(lstOpenCoords.size)
        (lstOpenCoords(idx), newRand)

  //T2 play - executa jogada se for valida

  def isValidCapture(board: Board, player: Stone, from: Coord2D, to: Coord2D, rows: Int, cols: Int): Boolean =
    val (fr, fc) = from
    val (tr, tc) = to
    val dr = tr - fr
    val dc = tc - fc
    val validDirection = (Math.abs(dr) == 2 && dc == 0) || (dr == 0 && Math.abs(dc) == 2)
    val inBounds = tr >= 0 && tr < rows && tc >= 0 && tc < cols
    if !validDirection || !inBounds then false
    else
      val mid = (fr + dr / 2, fc + dc / 2)
      board.get(from).contains(player) &&
        !board.contains(to) &&
        board.get(mid).contains(player.opponent)

  def executeCapture(board: Board, from: Coord2D, to: Coord2D, lstOpenCoords: List[Coord2D]): (Board, List[Coord2D]) =
    val (fr, fc) = from
    val (tr, tc) = to
    val mid = (fr + (tr - fr) / 2, fc + (tc - fc) / 2)
    val stone = board(from)
    val newBoard = (board - from - mid) + (to -> stone)
    val newOpen = from :: mid :: lstOpenCoords.filterNot(_ == to)
    (newBoard, newOpen)

  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) =
    val allCoords = board.seq.keySet ++ lstOpenCoords.toSet
    val (rows, cols) = if allCoords.isEmpty then (0, 0)
      else (allCoords.map(_._1).max + 1, allCoords.map(_._2).max + 1)
    findCapturePath(board, player, coordFrom, coordTo, rows, cols) match
      case Some(path) =>
        val (finalBoard, finalOpen) = executePath(board, player, path, lstOpenCoords)
        (Some(finalBoard), finalOpen)
      case None =>
        (None, lstOpenCoords)

  //procura caminho de captura
  def findCapturePath(board: Board, player: Stone, from: Coord2D, to: Coord2D, rows: Int, cols: Int): Option[List[Coord2D]] =
    if from == to then None
    else
      @tailrec
      def bfs(queue: List[(Coord2D, Board, List[Coord2D])], visited: Set[Coord2D]): Option[List[Coord2D]] =
        queue match
          case Nil => None
          case (current, currentBoard, path) :: rest =>
            val jumps = directions.flatMap { case (dr, dc) =>
              val next = (current._1 + dr * 2, current._2 + dc * 2)
              if !visited.contains(next) && isValidCapture(currentBoard, player, current, next, rows, cols) then
                val mid = (current._1 + dr, current._2 + dc)
                val newBoard = (currentBoard - current - mid) + (next -> currentBoard(current))
                Some((next, newBoard, path :+ next))
              else None
            }
            val found = jumps.find(_._1 == to)
            found match
              case Some((_, _, p)) => Some(p)
              case None =>
                val newVisited = visited ++ jumps.map(_._1)
                bfs(rest ++ jumps, newVisited)
      bfs(List((from, board, List(from))), Set(from))

  // Overload for external callers without dimensions
  def findCapturePath(board: Board, player: Stone, from: Coord2D, to: Coord2D): Option[List[Coord2D]] =
    val (rows, cols) = inferDimensions(board)
    findCapturePath(board, player, from, to, rows, cols)
//execcuta se existir

  def executePath(board: Board, player: Stone, path: List[Coord2D], lstOpenCoords: List[Coord2D]): (Board, List[Coord2D]) =
    path.zip(path.drop(1)).foldLeft((board, lstOpenCoords)) { case ((b, open), (from, to)) =>
      executeCapture(b, from, to, open)
    }

  //T3 playRandomly - jogada automatica

  def playRandomly(
    board: Board,
    r: MyRandom,
    player: Stone,
    lstOpenCoords: List[Coord2D],
    f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)
  ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) =
    val allCoords = board.seq.keySet ++ lstOpenCoords.toSet
    val (rows, cols) = if allCoords.isEmpty then (0, 0)
      else (allCoords.map(_._1).max + 1, allCoords.map(_._2).max + 1)
    val validMoves = getValidMoves(board, player, rows, cols)
    validMoves match
      case Nil => (None, r, lstOpenCoords, None)
      case moves =>
        val allDestinations = moves.flatMap(_._2).distinct
        if allDestinations.isEmpty then (None, r, lstOpenCoords, None)
        else
          val (chosen, newRand) = f(allDestinations, r)
          val sourceMoves = moves.filter(_._2.contains(chosen))
          sourceMoves match
            case Nil => (None, newRand, lstOpenCoords, None)
            case (from, _) :: _ =>
              val (result, newOpen) = play(board, player, from, chosen, lstOpenCoords)
              result match
                case Some(newBoard) => (Some(newBoard), newRand, newOpen, Some(chosen))
                case None => (None, newRand, lstOpenCoords, None)

  //onde teremos de implementar T5

  def getValidMoves(board: Board, player: Stone, rows: Int, cols: Int): List[(Coord2D, List[Coord2D])] =
    board.seq.toList
      .collect { case (coord, stone) if stone == player => coord }
      .map(from => (from, getJumpDestinations(board, player, from, rows, cols)))
      .filter(_._2.nonEmpty)

  // Overload for external callers without dimensions
  def getValidMoves(board: Board, player: Stone): List[(Coord2D, List[Coord2D])] =
    val (rows, cols) = inferDimensions(board)
    getValidMoves(board, player, rows, cols)

  def getJumpDestinations(board: Board, player: Stone, from: Coord2D, rows: Int, cols: Int): List[Coord2D] =
    getAllChainDestinations(board, player, from, Set(from), rows, cols)

  def getJumpDestinations(board: Board, player: Stone, from: Coord2D): List[Coord2D] =
    val (rows, cols) = inferDimensions(board)
    getJumpDestinations(board, player, from, rows, cols)

  def getAllChainDestinations(board: Board, player: Stone, from: Coord2D, visited: Set[Coord2D], rows: Int, cols: Int): List[Coord2D] =
    @tailrec
    def explore(queue: List[(Coord2D, Board, Set[Coord2D])], acc: List[Coord2D]): List[Coord2D] =
      queue match
        case Nil => acc
        case (current, currentBoard, vis) :: rest =>
          val jumps = directions.flatMap { case (dr, dc) =>
            val to = (current._1 + dr * 2, current._2 + dc * 2)
            if isValidCapture(currentBoard, player, current, to, rows, cols) && !vis.contains(to) then
              val mid = ((current._1 + to._1) / 2, (current._2 + to._2) / 2)
              val newBoard = (currentBoard - current - mid) + (to -> currentBoard(current))
              Some((to, newBoard, vis + to))
            else None
          }
          explore(rest ++ jumps, acc ++ jumps.map(_._1))
    explore(List((from, board, visited)), Nil)



  def getSingleJumpDestinations(board: Board, player: Stone, from: Coord2D, rows: Int, cols: Int): List[Coord2D] =
    directions.flatMap { case (dr, dc) =>
      val to = (from._1 + dr * 2, from._2 + dc * 2)
      if isValidCapture(board, player, from, to, rows, cols) then Some(to) else None
    }

  def getSingleJumpDestinations(board: Board, player: Stone, from: Coord2D): List[Coord2D] =
    val (rows, cols) = inferDimensions(board)
    getSingleJumpDestinations(board, player, from, rows, cols)

  //T4 representar visualmente o board , para texto

  def boardToString(board: Board, rows: Int, cols: Int): String =
    val colHeaders = (0 until cols).map(c => ('A' + c).toChar).mkString("  ", " ", "")
    val rowStrings = (0 until rows).toList.foldRight(List.empty[String]) { (r, acc) =>
      val cells = (0 until cols).map { c =>
        board.get((r, c)) match
          case Some(Stone.Black) => "B"
          case Some(Stone.White) => "W"
          case None => "."
      }.mkString(" ")
      s"$r $cells" :: acc
    }
    (colHeaders :: rowStrings).mkString("\n")

