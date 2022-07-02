package module1

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant
import scala.language.postfixOps



/**
 * referential transparency
 */
 object referential_transparency{


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }


  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(val notificationService: NotificationService) extends AbiturientService{
    override def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient = {
      val notification = Notification.Email("", "")
      val abiturient = Abiturient(UUID.randomUUID().toString, abiturientDTO.email, abiturientDTO.fio)
      //notificationService.sendNotification(notification)
      // save(abiturient)
      abiturient
    }
  }


}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = {
    if( n <= 0) 1 else n * factRec(n - 1)
  }

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if( n <= 1) accum
      else loop(n - 1, n * accum)
    loop(n, 1)
  }



  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{

   trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()

   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * Реализовать ф-цию, которая будет четать записи Request из топика, и сохранять их в базу
   */
   def createRequestSubscription() = {
     val cons : Consumer = ???
     val stream: LazyList[Record] = cons.subscribe("request")
     stream.foreach{ rec =>
       val req = Request.parse(rec.value)
       // save(req)
     }
   }

  def createSubscription(topic: String)(action: Record => Unit) = {
    val cons : Consumer = ???
    val stream: LazyList[Record] = cons.subscribe(topic)
    stream.foreach{ rec =>
     action(rec)
    }
  }

  def createRequestSubscription2() = createSubscription("request"){rec =>
    val req = Request.parse(rec.value)
    // save(req)
  }



  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result
  }






  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0



  def not[T](f: T => Boolean): T => Boolean = i => ! f(i)


  lazy val isEven = not(isOdd)




  // изменение самой функции

  // Follow type implementation

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val res: Int => Int = partial(1, sum)
  res(2) // sum(1, 2)


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

   sealed trait Option[+T]{
      def isEmpty: Boolean = this match {
        case Option.Some(v) => false
        case Option.None => true
      }

      def get: T = this match {
        case Option.Some(v) => v
        case Option.None => throw new Exception("Get on empty Option")
      }

      def map[B](f: T => B): Option[B] = this match {
        case Option.Some(v) => Option.Some(f(v))
        case Option.None => Option.None
      }

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny: Unit = this match {
      case Option.Some(v) => println(v)
      case Option.None => ()
    }

    /**
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = a match {
      case Option.Some(v) =>
        b match {
          case Option.Some(v1) => Option.Some((v, v1))
          case Option.None => Option.None
        }
      case Option.None => Option.None
    }

    /**
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(p: T => Boolean) = this match {
      case Option.Some(v) => if (p(v)) Option.Some(v) else Option.None
      case Option.None => Option.None
    }
  }

  object Option {
    case class Some[T](v: T) extends Option[T]
    case object None extends Option[Nothing]

    def apply[T](v: T): Option[T] = Some(v)
  }
}

object list {

  /**
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
   */

  sealed trait List[+T] {

    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     */
    def ::[A >: T](elem: A): List[A] = List.::(elem, this)

    /**
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     */
    def mkString(separator: String): String = {
      @tailrec
      def mkStringTail(res: String, list: List[T]): String = {
        list match {
          case List.Nil => res
          case List.::(last, List.Nil) => mkStringTail(res + s"$last", List.Nil)
          case List.::(head, tail) => mkStringTail(res + s"$head$separator", tail)
        }
      }
      mkStringTail("", this)
    }

    /**
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: List[T] = {
      @tailrec
      def reverseT(res: List[T], list: List[T]): List[T] = {
        list match {
          case List.::(head, tail) => reverseT(res.::(head), tail)
          case List.Nil => res
        }
      }
      reverseT(List.Nil, this)
    }

    /**
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[B](f: T => B): List[B] = {
      @tailrec
      def mapT(res: List[B], list: List[T]): List[B] =
        list.reverse match {
          case List.::(head, tail) => mapT(res.::(f(head)), tail)
          case List.Nil => res
        }
      mapT(List.Nil, this)
    }

    /**
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    def filter(p: T => Boolean): List[T] = {
      @tailrec
      def filterT(res: List[T], lst: List[T]): List[T] = {
        lst match {
          case List.::(head, tail) if p(head) => filterT(res.::(head), tail)
          case List.::(head, tail) if !p(head) => filterT(res, tail)
          case List.Nil => res
        }
      }
      filterT(List.Nil, this).reverse
    }
  }

  object List {
    case class ::[A](head: A, tail: List[A]) extends List[A]
    case object Nil extends List[Nothing]

    /**
     * Конструктор, позволяющий создать список из N - го числа аргументов
     * Для этого можно воспользоваться *
     *
     * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
     * def printArgs(args: Int*) = args.foreach(println(_))
     */
    def apply[T](args: T*): List[T] = {
      @tailrec
      def mkLst(res: List[T], seq: Seq[T]): List[T] = {
        if (seq.isEmpty) res else mkLst(res.::(seq.head), seq.tail)
      }
      mkLst(List.Nil, args.reverse)
    }

    /**
     * Написать функцию incList котрая будет принимать список Int и возвращать список,
     * где каждый элемент будет увеличен на 1
     */
    def incList(lst: List[Int]): List[Int] = lst.map(_ + 1)

    def flatMap[B](f: T => List[B]): List[B] = {
      type C = List[B]
      @tailrec
      def flatMapT(res: List[C], list: List[T]): List[C] = {
        list.reverse match {
          case List.::(head, tail) => flatMapT(res.::(f(head)), tail)
          case List.Nil => res
        }
      }


      @tailrec
      def flatten(res: List[B], origin: List[List[B]]): List[B] = {
        origin.reverse match {
          case List.::(head, tail) => flatten(head.reverse, tail)
          case List.Nil => res
        }
      }
      flatten(List.Nil, flatMapT(List.Nil, this))
    }

    /**
     * Написать функцию shoutString котрая будет принимать список String и возвращать список,
     * где к каждому элементу будет добавлен префикс в виде '!'
     */
    def shoutString(lst: List[String]): List[String] = lst.map(str => s"!$str")
  }
}
