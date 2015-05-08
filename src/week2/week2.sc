// Implementation of cons for Streams
trait Stream[T] {
  def head: T
  def tail: Stream[T]
}

def cons[T](hd: T, tl: => Stream[T] ) = new Stream[T] {
  def head = hd
  private var cache : Option[Stream[T]] = None
  def tail: Stream[T] = cache match {
    case Some(x) => x
    case None =>
      cache = Some(tl)
      tail
  }
}

// we can emulate a loop through a function
// We need to pass condition and command by name
// since we need to re-re-evaluate these two on every
// iteration.
def WHILE(condition: => Boolean)(command: => Unit): Unit = {
  if (condition) {
    command
    WHILE(condition)(command)
  } else ()
}

var i = 0
while (i < 10) {
  i += 1
  println(i)
}

def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (condition)
    REPEAT(command)(condition)
}

REPEAT {
  println(i)
  i += 1
} ( i < 10)


