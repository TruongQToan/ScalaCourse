package Week4

trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    def at(n: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
    def at(n: Int): T = 
        if (n < 0) throw IndexOutOfBoundsException()
        else if (n == 0) head
        else tail at (n - 1)
}

class Nil[T] extends List[T] {
    def isEmpty = true
    def head = throw new NoSuchElementException("Nil.head")
    def tail = throw new NoSuchElementException("Nil.head")
    def at(n: Int): T = throw new IndexOutOfBoundsException()
}
