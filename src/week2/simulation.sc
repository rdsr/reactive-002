abstract class Simulation {
  type Action = () => Unit
  case class Event(time: Int, action: Action)

  private var curTime = 0
  def currentTime: Int = curTime

  private var agenda: List[Event] = List()
  private def insert(ag: List[Event], item: Event): List[Event] = {
    ag match {
      case first :: rest if (first.time <= item.time) => first :: insert(rest, item)
      case _ => {
        //println("Inserted item " + item + " into agenda")
        item :: ag
      }
    }
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(delay, () => block)
    agenda = insert(agenda, item)
  }

  def run(): Unit = {
    afterDelay(0) {
      println("Starting simulation " + " at time " + curTime)
    }
    loop()
  }

  def loop(): Unit = {
    agenda match {
      case first :: rest => {
        agenda = rest
        curTime += first.time
        println("updated current time to " + curTime)
        first.action()
        loop()
      }
      case _ =>
    }
  }
}

trait Parameters {
  def inverterDelay = 1
  def andGateDelay = 2
  def orGateDelay = 3
}

abstract class Gates extends Simulation {
  def inverterDelay: Int
  def andGateDelay: Int
  def orGateDelay: Int

  class Wire {
    private var sigVal = false;
    private var actions: List[Action] = List()
    def getSignal: Boolean = sigVal
    def setSignal(s: Boolean): Unit = {
      if (s != sigVal) {
        sigVal = s
        actions foreach(_())
      }
    }
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def inverterAction(): Unit = {
      val s = input.getSignal
      afterDelay(inverterDelay) {
        output.setSignal(!s)
      }
    }
    input addAction inverterAction
  }

  def andGate(a: Wire, b: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val as = a.getSignal
      val bs = b.getSignal
      afterDelay(andGateDelay) {
        output setSignal (as & bs)
      }
    }
    a addAction andAction
    b addAction andAction
  }

  def orGate(a: Wire, b: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val as = a.getSignal
      val bs = b.getSignal
      afterDelay(orGateDelay) {
        output setSignal (as | bs)
      }
    }

    a addAction andAction
    b addAction andAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def action(): Unit = {
      println("name " + " " + name + " new-value " + wire.getSignal)
    }
    wire addAction action
  }
}

abstract class Circuits extends Gates {
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, s: Wire, cout: Wire): Unit ={
    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, s, c2)
    orGate(c1, c2, cout)
  }
}

object sim extends Circuits with Parameters
import sim._
val in1, in2, sum, carry = new Wire
halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)
in1 setSignal true
run()
in2 setSignal true
run()
