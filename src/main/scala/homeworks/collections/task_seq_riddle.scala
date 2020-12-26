package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] =
    currentLine
      .foldRight(Nil: List[(Int, Int)]) {
        (next, acc) => acc match {
          case (cnt, value) :: tail if value == next => (cnt + 1, value) :: tail
          case _                                     => (1, next) :: acc
        }
      }
      .flatMap { case (a, b) => List(a, b) }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = LazyList.cons(List(1), funSeq.map(nextLine))
}
