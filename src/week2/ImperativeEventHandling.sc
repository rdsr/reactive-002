trait Subscriber {
  def handler(pub: Publisher)
}

trait Publisher {
  private var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber

  def unsubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber

  def publish(): Unit =
    subscribers.foreach(_.handler(this))
}

class BankAccount extends Publisher {
  private var balance = 0

  def currentBalance = balance

  def withdraw(amount: Int): Unit = {
    if (amount > 0 && balance > amount) {
      balance -= amount
      publish()
    }
    throw new Error("Insufficient funds")
  }

  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      balance += amount
    }
    publish()
  }
}