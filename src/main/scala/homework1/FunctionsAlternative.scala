package homework1

import scala.annotation.tailrec

def fromDigitsAlt(digits: List[Int], radix: Int = 10): Int =
  @tailrec
  def fromDigits(digits: List[Int], acc: Int): Int =
    if digits.isEmpty then acc
    else fromDigits(digits.tail, acc * radix + digits.head)

  fromDigits(digits, 0)

def parseIntegerAlt(integer: String, radix: Int = 10): Int =
  def toNumericValue(digit: Char): Int =
    if '0' <= digit && digit <= 9 then digit - '0'
    else if 'A' <= digit && digit <= 'Z' then digit - 'A'
    else throw new IllegalArgumentException(s"$digit is not a valid digit")

  if integer.nonEmpty && integer.head == '-' then -1 * parseInteger(integer.tail, radix)
  else fromDigits(integer.toList.map(toNumericValue), radix)

def zipMapAlt[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
  @tailrec
  def zipMap(a: List[A], b: List[B], acc: List[C]): List[C] =
    if a.isEmpty || b.isEmpty then acc.reverse
    else zipMap(a.tail, b.tail, f(a.head, b.head) :: acc)

  zipMap(a, b, List.empty)

def memoize[A, B, R](f: (A, B) => R): (A, B) => R =
  val cache = scala.collection.mutable.Map.empty[(A, B), R]

  (a, b) => cache.getOrElseUpdate((a, b), f(a, b))

def countCoinChangeVariantsAlt(denominations: Set[Int], change: Int): Int =
  lazy val countCoinChangeVariantsMemoized: (Set[Int], Int) => Int = memoize: (denominations, change) =>
    if change == 0 then 1
    else if change < 0 || denominations.isEmpty then 0
    else
      countCoinChangeVariantsMemoized(denominations, change - denominations.head) +
        countCoinChangeVariantsMemoized(denominations.tail, change)

  countCoinChangeVariantsMemoized(denominations, change)

def countCoinChangeVariantsDynamicProgramming1(denominations: Set[Int], change: Int): Int =
  val countWithTargetSum = for
    coin <- denominations.view
    targetSum <- coin to change
  yield (coin, targetSum)

  val sumToCounts = countWithTargetSum.foldLeft(Map(0 -> 1).withDefaultValue(0)): (sumToCounts, coinToCount) =>
    val (coin, targetSum) = coinToCount

    sumToCounts.updated(targetSum, sumToCounts(targetSum) + sumToCounts(targetSum - coin))

  sumToCounts.getOrElse(change, 0)

def countCoinChangeVariantsDynamicProgramming2(denominations: Set[Int], change: Int): Int =
  @tailrec
  def loop(denominations: Set[Int], sumToCountsSoFar: Map[Int, Int]): Map[Int, Int] =
    if denominations.isEmpty then sumToCountsSoFar
    else
      val nextCoin = denominations.head

      val sumToCountsAfterCoinIsApplied = (nextCoin to change).foldLeft(sumToCountsSoFar): (sumToCounts, targetSum) =>
        sumToCounts.updated(targetSum, sumToCounts(targetSum) + sumToCounts(targetSum - nextCoin))

      loop(denominations.tail, sumToCountsAfterCoinIsApplied)

  loop(denominations, Map(0 -> 1).withDefaultValue(0)).getOrElse(change, 0)

def combinationsAlt[A](xs: List[A], n: Int): List[List[A]] =
  if n == 0 then List(List.empty)
  else if xs.isEmpty then List.empty
  else
    val combinationsWithHead = combinationsAlt(xs.tail, n - 1).map(xs.head :: _)
    val combinationsWithoutHead = combinationsAlt(xs.tail, n)

    combinationsWithHead ::: combinationsWithoutHead
