object SideEffects {

  class Cake
  class CreditCard

  def pay(card: CreditCard, amount: Double): Boolean = ???

  def buyCake(card: CreditCard): Cake = {
    pay(card, 1.49)
    new Cake
  }

  val myCreditCard = new CreditCard
  val chocolateCake = buyCake(myCreditCard)
  val cheeseCake = buyCake(myCreditCard)
  val battenburg = buyCake(myCreditCard)
}




















object NoSideEffects {

  class Cake
  class CreditCard

  case class Payment(card: CreditCard, amount: Double)

  sealed trait PaymentResult

  def pay(payment: Payment): PaymentResult = ???

  def buyCake(payment: CreditCard): (Payment, Cake) =
    (Payment(card, 1.49), new Cake)

  val myCreditCard = new CreditCard
  val (payment1, chocolateCake) = buyCake(myCreditCard)
  val (payment2, cheeseCake) = buyCake(myCreditCard)
  val (payment3, battenburg) = buyCake(myCreditCard)

  val results: Seq[PaymentResult] = Seq(payment1, payment2, payment3) map pay
}


























object NoSideEffectsBetter {
  class Cake
  class CreditCard

  case class Payment(card: CreditCard, amount: Double)

  sealed trait PaymentResult

  def pay(payment: Payment): PaymentResult = ???

  def buyCake(payment: CreditCard): (Payment, Cake) =
    (Payment(card, 1.49), new Cake)

  val myCreditCard = new CreditCard
  val (payments, cakes) = List.fill(3){buyCake(myCreditCard)}.unzip
  val oneBigPayment: Payment = payments.reduce { (p1, p2) => Payment(p1.card, p1.amount + p2.amount) }

  val result = pay(oneBigPayment)
}


















object Exceptions {

  class CreditCard
  case class Payment(card: CreditCard, amount: Double)

  case class Receipt

  def pay(payment: Payment): Receipt = {

    if(authorise(payment.card)) {
      generateReceipt(Receipt)
    } else {
      throw Exception("Payment failed")
    }
  }


  val myCreditCard = new CreditCard
  val myPayment = Payment(myCreditCard)

}







































object NullBank {

 case class User(userName: String)
 case class Account(balance: Double, user: User)
 type Error = String

 def authorise(userName: String, password: String): User = ???
 def findAccount(user: User): Account = ???


 def getBalance(userName: String, password: String): Double = {
   val user = authorise(userName, password)
   if(user != null) {
     val account = findAccount(user)
     if(account != null) {
       account.balance
     } else {
       null
     }
   } else {
     null
   }
 }

}











object Options {

 sealed trait Option[A] {

   // If we have a defined value, return that, otherwise return a default
   def getOrElse(default: A): A this match {
     case Some(a) => a
     case None    => default
   }

   // If we have a defined value, pass it to a given function and return
   // a new Option containing the new value, otherwise return None
   def map[B](func: A => B): Option[B] = this match {
     case Some(a) => Some(func(a))
     case None    => None
   }

   // As map, but the given function is required to return and instance
   // of Option
   def flatMap[B](func: A => Option[B]): Option[B] = this match {
     case Some(a) => func(a)
     case None    => None
   }
 }

 case class Some(get: A) extends Option[A]
 case object None extends Option[Nothing]

}

















object UsingOptions {

  case class Employee(name: String, wage: Double, role: String)
  case class Payment(employee: Employee, amount: Double)

  // getOrElse
  def payEmployee(employee: Employee, bonus: Option[Double]): Payment =
    Payment(employee, employee.wage + bonus.getOrElse(0))

  // map
  def findEmployee(name: String): Option[Employee] =
    // TODO: Some code to try and find the employee
    if(found)
      Some(foundEmployee)
    else
      None

  def findEmployeeRole(name: String): Option[String] =
    findEmployee(name).map( employee => employee.role)
}
























object OptionBank {

 case class User(userName: String)
 case class Account(balance: Double, user: User)
 type Error = String

 def authorise(userName: String, password: String): Option[User] = ???
 def findAccount(user: User): Option[Account] = ???

 // Hmm, doesn't seem to work...
 def getBalance(userName: String, password: String): Option[Double] =
   val accountOptOpt: Option[Option[Account]] =
     authorise(userName, password).map(user => findAccount(user).map(account => account.balance) )
 }






 // flatMap to the rescue!
 def getBalance(userName: String, password: String): Option[Double] =
   authorise(userName, password) flatMap { user =>
     findAccount(user) map { account =>
       account.balance
     }
   }

 def getBalance(userName: String, password: String): Option[Double] =
   authorise(userName, password) flatMap findAccount map (_.balance)


 }














object Eithers {

  sealed trait Either[A,B] {
    // If we have value of the a 'Right', pass it to a given function and
    // return a new Right containing the new value, if we have a 'Left', return
    // that as it is
    def map[B](func: A => B): Either[B] = this match {
      case Right(a) => Right(func(a))
      case _    => this
    }

    // As map, but the given function is required to return an instance
    // of Either
    def flatMap[B](func: A => Either[B]): Either[B] = this match {
      case Right(a) => func(a)
      case _    => this
    }
  }

  case class Left(get: A) extends Either[A,B]
  case class Right(get: B) extends Either[A,B]

}











object EitherBank {

  case class User(userName: String)
  case class Account(balance: Double, user: User)
  type Error = String

  def authorise(userName: String, password: String): Either[Error, User] = ???
  def findAccount(user: User): Either[Error, Account] = ???


  def getBalance(userName: String, password: String): Either[Error, Double] =
    authorise(userName, password).flatMap { user =>
      findAccount(user).map { account
        account.balance
      }
    }

}














// Only accepts the same type of monad

// Monads (in Scala) are only a design pattern...

object EitherForYieldBank {

  case class User(userName: String)
  case class Account(balance: Double, user: User)
  type Error = String

  def authorise(userName: String, password: String): Either[Error, User] = ???
  def findAccount(user: User): Either[Error, Account] = ???


  def getBalance(userName: String, password: String): Either[Error, Double] =
   for {
     user    <- authorise(userName, password)
     account <- findAccount(user)
   } yield account.balance

}

// Look for map and flatMap!
