package konane

import scala.collection.parallel.immutable.ParMap
import scala.annotation.tailrec

object Game:

  val directions: List[(Int, Int)] = List((-1, 0), (1, 0), (0, -1), (0, 1))

  // T2 inicializa board com pedras alternadas
  def initBoard(rows: Int, cols: Int): Board =
    val entries = for
      r <- (0 until rows).toList
      c <- (0 until cols).toList
      stone = if (r + c) % 2 == 0 then Stone.Black else Stone.White
    yield (r, c) -> stone
    ParMap(entries*)

  // T2 remove pedra preta e pedra branca do centro ou canto
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

  // T1: coordenada random de posicoes available
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) =
    lstOpenCoords match
      case Nil => throw new IllegalArgumentException("Empty list")
      case single :: Nil => (single, rand)
      case _ =>
        val (idx, newRand) = rand.nextInt(lstOpenCoords.size)
        (lstOpenCoords(idx), newRand)

  // T2: valida acao 
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

  // T2: executa uma acao unica
  def executeCapture(board: Board, from: Coord2D, to: Coord2D, lstOpenCoords: List[Coord2D]): (Board, List[Coord2D]) =
    val (fr, fc) = from
    val (tr, tc) = to
    val mid = (fr + (tr - fr) / 2, fc + (tc - fc) / 2)
    val stone = board(from)
    val newBoard = (board - from - mid) + (to -> stone)
    val newOpen = from :: mid :: lstOpenCoords.filterNot(_ == to)
    (newBoard, newOpen)

  // T2: executa todas as caps ao longo da path
  def executePath(board: Board, player: Stone, path: List[Coord2D], lstOpenCoords: List[Coord2D]): (Board, List[Coord2D]) =
    path.zip(path.drop(1)).foldLeft((board, lstOpenCoords)) { case ((b, open), (from, to)) =>
      executeCapture(b, from, to, open)
    }

  // T2: encontra path valida 
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
            jumps.find(_._1 == to) match
              case Some((_, _, p)) => Some(p)
              case None =>
                bfs(rest ++ jumps, visited ++ jumps.map(_._1))
      bfs(List((from, board, List(from))), Set(from))

  // T2: play - move peca se valida, devolve novo board e coordenadas
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

  // T2: saltinhos de forma a encontrar espacos perto
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

  // T3: saltos validos para o jogador
  def getValidMoves(board: Board, player: Stone, rows: Int, cols: Int): List[(Coord2D, List[Coord2D])] =
    board.seq.toList
      .collect { case (coord, stone) if stone == player => coord }
      .map(from => (from, getAllChainDestinations(board, player, from, Set(from), rows, cols)))
      .filter(_._2.nonEmpty)

  // T3: play randomly usando funcao f para escolher coords
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
        val origins = moves.map(_._1)
        val (chosenFrom, r2) = f(origins, r)
        val destinations = moves.find(_._1 == chosenFrom).map(_._2).getOrElse(Nil)
        destinations match
          case Nil => (None, r2, lstOpenCoords, None)
          case dests =>
            val (chosenTo, r3) = f(dests, r2)
            val (result, newOpen) = play(board, player, chosenFrom, chosenTo, lstOpenCoords)
            result match
              case Some(newBoard) => (Some(newBoard), r3, newOpen, Some(chosenTo))
              case None => (None, r3, lstOpenCoords, None)

  // T4: representacao do board para string 
  def boardToString(board: Board, rows: Int, cols: Int): String =
    val colHeaders = (0 until cols).toList.map(c => ('A' + c).toChar).mkString("  ", " ", "")
    val rowStrings = (0 until rows).toList.foldRight(List.empty[String]) { (r, acc) =>
      val cells = (0 until cols).toList.map { c =>
        board.get((r, c)) match
          case Some(Stone.Black) => "B"
          case Some(Stone.White) => "W"
          case None => "."
      }.mkString(" ")
      s"$r $cells" :: acc
    }
    (colHeaders :: rowStrings).mkString("\n")
