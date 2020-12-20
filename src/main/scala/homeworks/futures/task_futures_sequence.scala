package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.Await

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]]): Future[(List[A], List[Throwable])] = Future {
    var successes: List[A] = Nil
    var failures: List[Throwable] = Nil

    for ( f <- futures.reverse ) {
      try {
        successes = Await.result(f, Duration.Inf) :: successes
      }
      catch {
        case exception: Throwable => failures = exception :: failures
      }
    }

    (successes, failures)
  }
}
