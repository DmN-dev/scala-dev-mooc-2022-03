package module2

object homework_hkt_impllicts extends App {

    /**
      * 
      * Доработать сигнатуру tupleF и реализовать его
      * По итогу должны быть возможны подобные вызовы
      *   val r1 = println(tupleF(optA, optB))
      *   val r2 = println(tupleF(list1, list2))
      * 
      */
    def tupleF[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A,B)] =
      fa.flatMap(a => fb.map(b => (a,b)))


    trait Bindable[F[_], A] {
        def map[B](f: A => B): F[B]
        def flatMap[B](f: A => F[B]): F[B]
    }


  implicit class OptOps[A](opt: Option[A]) {
    def toBindable: Bindable[Option, A] = new Bindable[Option, A] {
      override def map[B](f: A => B): Option[B] = opt.map(f)
      override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
    }
  }

  implicit class ListOps[A](lst: List[A]) {
    def toBindable: Bindable[List, A] = new Bindable[List, A] {
      override def map[B](f: A => B): List[B] = lst.map(f)
      override def flatMap[B](f: A => List[B]): List[B] = lst.flatMap(f)
    }
  }

  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleF(optA.toBindable, optB.toBindable))
  val r2 = println(tupleF(list1.toBindable, list2.toBindable))
}