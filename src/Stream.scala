/**
  * Created by I024070 on 4/26/2017.
  * Dummy implementation of Stream
  */
trait StreamT {
  def isEmpty: Boolean
  def head: Any
  def tail: StreamT
}

object StreamT {
  def cons[T] (hd: T, tl: => StreamT) = new StreamT {
    def isEmpty = false
    def head = hd
    lazy val tail = tl //do not calculate tail until it's needed (have a call to it)
  }

  val empty = new StreamT {
    def isEmpty = true
    def head = throw new NoSuchElementException("empty.head")
    def tail = throw new NoSuchElementException("empty.tail")
  }
}
