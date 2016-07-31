object exercise {

  /**
    *  Exercise 2.1
    *   n번째 피보나치 수를 돌려주는 재귀 함수를 작성하라 처음 두 피보나치 수는 0과 1이다.
    *  n번째 피보나치 수는 항상 이전 두 수의 합이다. 즉, 피보나치수열은 0, 1, 2, 3, 5로
    *  시작한다 반드시 지역 꼬리 재귀 함수를 사용해서 작성할 것.
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, now: Int): Int = {
      if (n <= 0) now
      else loop(n - 1, now, prev + now)
    }
    loop(n, 0, 1)
  }

  fib(0)
  fib(1)
  fib(2)
  fib(3)
  fib(4)
  fib(5)

  /**
    *  Exercise 2.2
    *   Array[A]가 주어진 비교 함수에 의거해서 정렬되어 있는지 점검하는 isSorted 함수를
    *  구현하라.
    */
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x > y)
  isSorted(Array(1, 2, 5, 4, 3), (x: Int, y: Int) => x > y)

  /**
    *  Exercise 2.3
    *   또 다른 예로 인수가 두 개인 함수 f를 인수 하나를 받고 그것으로 f를 부분 적용하는 함수로
    *  변환하는 커링(currying)을 살펴보자 이번에는 컴파일되는 구현은 단 한가지이다 그러한 구현을
    *  작성하라
    *
    *   Note that '=>' associates to the right, so we could write
    *  the return type as 'A => B => C'
    *
    *   NB : The 'Function2' trait has a 'curried' method already,
    *  so if you wanted to cheat a little you could write the answer
    *  as f.curried
    */

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  /**
    *  Exercise 2.4
    *   curry의 변환을 역으로 수행하는 고차 함수 uncurry를 구현하라. => 오른쪽으로 묶이므로,
    *  A => (B => C)를 A => B => C 라고 표기할 수 있음을 주의할 것.
    *
    *   NB : There is a method on the 'Function' object in the standard library,
    *  'Function.uncurried' that you can use for uncurrying.
    *
    *   Note that we can go back and forth between the two forms. We can curry
    *  and uncurry and the two forms are in some sense "the same". In FP jargon,
    *  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
    *  a term we inherit from category theory.
    */

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  /**
    *  Exercise 2.5
    *   두 함수를 합성하는 고차 함수를 구현하라.
    */

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}