package homework1

import scala.annotation.tailrec

def fromDigits(digits: List[Int], radix: Int = 10): Int =
  digits.foldLeft(0)((acc, digit) => acc * radix + digit)

def parseInteger(integer: String, radix: Int = 10): Int =
  if integer.nonEmpty && integer.head == '-' then -1 * parseInteger(integer.tail, radix)
  else fromDigits(integer.toList.map(_.asDigit), radix)

def zipMap[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
  (a zip b).map(f.tupled)

def countCoinChangeVariants(denominations: Set[Int], change: Int): Int =
  if change == 0 then 1
  else if change < 0 || denominations.isEmpty then 0
  else
    countCoinChangeVariants(denominations, change - denominations.head) +
      countCoinChangeVariants(denominations.tail, change)

def combinations[A](xs: List[A], n: Int): List[List[A]] =
  if n == 0 then List(List.empty)
  else
    for
      case first :: rest <- xs.tails.toList
      restCombination <- combinations(rest, n - 1)
    yield first :: restCombination

def bfsTraversal(neighbours: Int => List[Int])(start: Int, end: Int): Queue[Int] =
  @tailrec
  def bfs(toVisit: Queue[Int], visited: Set[Int], path: Queue[Int]): Queue[Int] =
    if toVisit.isEmpty then path
    else
      val (current, nextToVisit) = toVisit.pop

      if current == end then path.push(end)
      else if visited(current) then bfs(nextToVisit, visited, path)
      else
        bfs(
          nextToVisit.push(neighbours(current)),
          visited + current,
          path.push(current)
        )

  bfs(Queue(start), Set.empty, Queue.empty)
