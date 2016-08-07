object exercise {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  /**
    *  Exercise 3.1
    *   다음 패런 부합 표션식의 결과는 무엇인가
    */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  /**
    *  Exercise 3.2
    *   List의 첫 요소를 제거하는 함수 tail을 구현하라 이 함수가 상수 시간으로 실행됨을
    *  주의할 것. Nil인 List도 지원하도록 도자의 구현을 수정하는 여러 가지 방법들도 고려해
    *  보라. 이에 대해선느 다음 장에서 좀 더 살펴볼 것이다
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(h, t) => t
  }

  tail(List(1, 2, 3, 4, 5))
//  tail((Nil))

  /**
    *  Exercise 3.3
    *   같은 맥락에서 List의 첫 요소를 다른 값으로 대체하는 함수 setHead를 구현하라.
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Nil don't have head")
    case Cons(_,t) => Cons(h,t)
  }

  setHead(List(1, 2, 3), 5)
//  setHead(Nil, 3)

  /**
    *  Exercise 3.4
    *   tail을 일반화해서 목록에서 처음 n개의 요소를 제거하는 함수 drop을 구현하라. 이 함수의
    *  실행시간은 제거되는 원소의 개수에만 비례함을 주의할 것. List 전체의 복사본을 만들 필요는
    *  없다
    *
    *  Hint.
    *
    *   What should the function do if the `n` argument is 0?
    *   What should it do if the list is empty?
    *   What if the list is not empty and `n` is nonzero?
    *   Consider all of these cases.
    *   Use pattern-matching and recursion.
    */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(h,t) => drop(t, n-1)
    }

  drop(List(1, 2, 3, 4, 5), 3)

  /**
    *  Exercise 3.5
    *   주어진 술어(predicate)와 부합하는 List의 앞 요소들(prefix)을 제거하는 함수
    *  dropWhile을 구현하라
    *
    *  Hint.
    *
    *   What should the function do if the list is empty?
    *   What if it's not empty?
    *   Use pattern-matching and recursion.
    *
    *  Somewhat overkill, but to illustrate the feature we're
    *  using a _pattern guard_,to only match a `Cons` whose head satisfies
    *  our predicate, `f`. The syntax is to add `if <cond>` after the pattern,
    *  before the `=>`, where `<cond>` can use any of the variables introduced
    *  by the pattern.
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  dropWhile(List(1, 2, 3), (x: Int) => x == 1)
  dropWhile(List(1, 2, 3, 1), (x: Int) => x == 1)

  /**
    *  Exercise 3.6
    *   그러나 모든 것이 효율적이지는 않다 한 List의 마지막 요소를 제외한 모든 요소로 이루어진
    *  List를 돌려주는 함수 init을 구현하라 예를 들어 List(1,2,3,4)에 대해 init은
    *  List(1,2,3)을 돌려주어야 한다 이 함수를 tail처럼 상수 시간으로 구현할 수 없는 이유는
    *  무엇인가?
    *
    *  Hint.
    *
    *   What should the function do if the list is empty?
    *   What if it's not empty?
    *   Use pattern-matching and recursion.
    *
    *  Note.
    *
    *    that we're copying the entire list up until the last element.
    *   Besides being inefficient, the natural recursive solution will use
    *   a stack frame for each element of the list, which can lead to stack
    *   overflows for large lists (can you see why?). With lists, it's common
    *   to use a temporary, mutable buffer internal to the function (with lazy
    *   lists or streams, which we discuss in chapter 5, we don't normally do
    *   this). So long as the buffer is allocated internal to the function,
    *   the mutation is not observable and RT is preserved.
    *
    *    Another common convention is to accumulate the output list in
    *   reverse order, then reverse it at the end, which doesn't require
    *   even local mutation. We'll write a reverse function later in this
    *   chapter.
    */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("empty list")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  init(List(1, 2, 3, 4, 5))
}
