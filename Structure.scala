
// 関数型データ構造は基本的にimmutable
// でもデータを余分にコピーするわけではない???
// List() or Nil
// a ++ b: list append

// パラメータ化された[+A]
// データ型はtraitを用いて定義する
// sealed: このtraitの実装がファイルで閉じている
// 関数同様データ構造も多相
// +はAが共変(covariant)であることを意味
sealed trait List[+A]
case object Nil extends List[Nothing]
// Cons: constructの略
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// traitに対するコンパニオンオブジェクト
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    // (0.0, _)は(x, xs)に含まれ、0.0を返すので冗長
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // sumとproductは似てる
  // 部分式を関数の引数として抽出すると一般化できる
  // 異なる部分: Nilの場合(Listの終端)の値, 演算子
  // +,*はxとxsを参照
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z, h))(f)
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
  // (x, y) => x * y == _ * _
  // _表記:e.g _.head, _ drop _ => xs.head, (xs, n) => xs.drop(n)
  // むやみに使用しない
  def product2(ds: List[Double]) = foldRight(ds, 1.0)(_ * _) 

  def length[A](as: List[A]): Int = foldRight(as, 0)((_,acc) => acc + 1)

  // A*: 可変長
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, x) => x
  }

  def drop[A](l: List[A], n: Int): List = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
  }


  // dropWhileの引数x
  // val xs: List[Int] = List(1, 2, 3)
  // val ex1 = dropWhile(xs, (x: Int) => x < 3)
  // xの型を示す必要がある
  // Scalaの型推論の問題
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      // dropWhile(xs)が関数を返し、それを引数fで呼び出す
      // curry化
      // 引数のグルーピングは型推論の手助けになる
      // dropWhile(xs)(x => x < 4)
      // 関数引数を複数の引数リストにまとめて型推論を最適化する
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => as
    }

  // a1の長さ依存
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
  }

  // tailのように一定時間で実装出来ない
  // 単方向リストの構造の問題
  // それより手前のConsオブジェクトの全てのコピーが必要
  // Vector: ランダムアクセス、更新、head, tail, initが一定時間で可能
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => // sys.error()
      case (h, Nil) => h
      case (h, t) => init(h, init(t))
    }

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  def double2String(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((h,t) => Cons(f(h),t))

  // TODO
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)
}

object Main {
  def main() = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
  }
}
