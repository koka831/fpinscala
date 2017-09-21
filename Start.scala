// commnet
/* comment */
/** document comment */


// object : Singleton
// Javaでstaticメンバを持つクラスを使用するような場面で
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

    private def formatAbs(x: Int) = {
      val msg = "The absolute value of %d is %d"
      msg.format(x, abs(x))
    }

    private def formatResult(name: String, n: Int, f: Int => Int) = {
      val msg = "The %s of %d is %d."
      msg.format(name, n, f(n))
    }

    def main(args: Array[String]): Unit =
      println(formatAbs(-43))

    def factorial(n: Int): Int = {
      /** local definition */
      // 再帰ヘルパー関数は通例で
      // def go/ def loop
      // tailrec: 末尾呼び出しであるかどうか、コンパイル時チェック
      @annotation.tailrec
      def go(n: Int, acc: Int): Int =
        if (n <= 0) acc
        else go(n-1, n*acc)

        go(n, 1)
    }

    def fib(n: Int): Int = {
      // local helper definition
      @annotation.tailrec
      def loop(a: Int, b: Int, n: Int): Int =
        if (n == 0) a
        else loop(b, a+b, n-1)

        loop(0, 1, n)
    }

  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

      loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): (B => C) = {
    (b: B) => f(a, b)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a,b) => f(a)(b)
  }

  // composeはよく使用されるので、stdlibに定義されてる
  // f compose g
  // f andThen g == g compose f
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}

