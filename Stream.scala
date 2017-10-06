
sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
// not EmptyなStreamは先頭と末尾で構成され、どちらも非正格
case class Cons[+A](h: () => A, t:() => Stream[A]) extends Stream[A]


// StreamはListと似ているが、Consデータコンストラクタは通常の正格値ではなく
// // 評価されない形式の式をthunk(サンク)
// 明示的なサンクを受け取る() => A と() => Stream[A]
object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    // lazy keyword: 右辺の評価が最初に参照されるまで先送りされる
    // また、その後の参照で再度評価されることがないようキャッシュされる
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail:_*))

  def headOption: Option[A] = this match {
    case Empty => None
    // h()を用いてhサンクを強制評価
    case Cons(h,t) => Some(h())
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] =
      s match {
        case Cons(h,t) => go(t(), h() :: acc)
        case _ => acc
      }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h,_) if n == 1 => cons(h(), empty)
    case _ => empty
  }
}
