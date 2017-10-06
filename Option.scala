

object OptionSample {

  // 呼び出し元がエラーテェックをしなくても
  // コンパイルが通るため、その後のコードの
  // 動作が保証できない
  // 多相コードに適用できない
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("")
    else xs.sum / xs.length

  // 入力の処理が出来なかった場合を引数で渡す
  // 呼び出し元依存が大きい
  // 呼び出し元でmeanが未定義の場合にはonEmptyで
  // 処理を分けることをしたくない
  def mean_1(xs: Seq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length
}

sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

// 呼び出し側がOptionのハンドリングをする必要があるため、
// コンパイル時チェックができる
object Option {

  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Listの全ての関数をコンパニオンオブジェクトにまとめたが、
  // Optionを利用するもので可能な場合にはOptiontraitの内部でdef
}

trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
  }

  // B>:A はBがAのスーパークラスである
  def getOrElse[B>:A](default: => B): B = this match {
    case None => None
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob
}
