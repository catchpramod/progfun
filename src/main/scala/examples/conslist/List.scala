package examples.conslist

/**
  * Created by pramod on 5/29/2016.
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def get(index: Int) : T = {
    def loop(n: Int, list: List[T]): T= {
      if (list.isEmpty) throw new IndexOutOfBoundsException()
      else if (n == 0) list.head
      else loop(n-1,list.tail)
    }
    loop(index,this)
  }
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false

}

class Nil[T] extends List[T]{
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchMethodException("Nil has no head!")
  override def tail: Nothing = throw new NoSuchMethodException("Nil has no tail!")
}

object Test{
  def main(args: Array[String]) {
    val list = new Cons(1, new Cons(2,new Cons(3, new Nil)))
    val l1 = new Nil
    println(list.get(2))
    println(l1.get(0))
  }
}