//   http://blog.tmorris.net/revised-scala-exercises/


sealed trait MList[+A] {
  override def toString = {
    def toScalaList(t: MList[A]): scala.List[A] = t match {
      case Empty => Nil
      case Cons(h, t) => h :: toScalaList(t)
    }
    toScalaList(this).toString
  }
}
final case object Empty extends MList[Nothing]
final case class Cons[A](h: A, t: MList[A]) extends MList[A]
 
object MList {
  def foldRight[A, B](as: MList[A], b: B, f: (A, B) => B): B = as match {
    case Empty => b
    case Cons(h, t) => f(h, foldRight(t, b, f))
  }
 
  def foldLeft[A, B](as: MList[A], b: B, f: (B, A) => B): B = as match {
    case Empty => b
    case Cons(h, t) => foldLeft(t, f(b, h), f)
  }
 
  def reduceRight[A](as: MList[A], f: (A, A) => A): A = as match {
    case Empty => error("bzzt. reduceRight on empty list")
    case Cons(h, t) => foldRight(t, h, f)
  }
 
  def reduceLeft[A](as: MList[A], f: (A, A) => A): A = as match {
    case Empty => error("bzzt. reduceLeft on empty list")
    case Cons(h, t) => foldLeft(t, h, f)
  }
 
  def unfold[A, B](b: B, f: B => Option[(A, B)]): MList[A] = f(b) match {
    case Some((a, b)) => Cons(a, unfold(b, f))
    case scala.None => Empty
  }
}
 
sealed trait Natural {
  override def toString = {
    def toInt(n: Natural): Int = n match {
      case Zero => 0
      case Succ(x) => 1 + toInt(x)
    }
    toInt(this).toString
  }
  
    
}
final case object Zero extends Natural
final case class Succ(c: Natural) extends Natural
 
object Exercises {
  
  
  def main(args: Array[String]) {     
    
  
    
  //println(    Succ(Succ(Succ(Succ(Succ(Zero))))).lessThan(Succ(Succ(Zero)))     )   
    println("blah")
  //   println( add(  Succ(Succ(Zero))  , Succ(Zero)))
    //println( sum(Cons(3,Cons(5,Empty)  ))  )
    
  //  println( length(Cons(3,Cons(5,Empty)  ))  )
  //  println( length(Empty  )  )
    
  //  println(map( Cons(3,Cons(5,Empty)),(x: Int) => x + 1   ))
  //  println(filter( Cons(2,Cons(6,Empty)),(x: Int) => x.%(2)==0   ))
    println(append( Cons(1,Cons(2,Empty)),Cons(3,Cons(4,Empty))   ))
      println(append( Cons(1,Cons(2,Empty)),Empty))
        println(append( Empty,Cons(3,Cons(4,Empty))   ))
        //println("flatten")
           //println(flatten(        )
        println("flatMap")
     println(flatMap( Cons("one",Cons("three",Empty)) ,     (x: String) =>   Cons(2,Cons(1,Empty))  ))
     println("map")
      println(map( Cons("one",Cons("three",Empty)) ,     (x: String) =>   Cons(2,Cons(1,Empty))  ))
      
      println("flatten")
      println(flatten (   map( Cons("one",Cons("three",Empty)) ,     (x: String) =>   Cons(2,Cons(1,Empty))  ) ))
    println("maximum")
     println(maximum( Cons(2,Cons(1,Empty))))
       println(maximum( Empty))
       
       println("reverse")
        println(reverse( Cons(2,Cons(1,Empty))))
    }
 
//DOJO 
  
// Exercise 1
// Relative Difficulty: 1
// Correctness: 2.0 marks
// Performance: 0.5 mark
// Elegance: 0.5 marks
// Total: 3
def add(x: Natural, y: Natural): Natural = {
   x match {
  case Zero => y 
  case Succ(b) =>   Succ( add(b,y)  )
}
   
  
 
 
  

   
}
 
// Exercise 2
// Relative Difficulty: 2
// Correctness: 2.5 marks
// Performance: 1 mark
// Elegance: 0.5 marks
// Total: 4
def sum(is: MList[Int]): Int = {
  MList.foldLeft(is, 0, (x: Int,y : Int) =>  x  + y)
}
 
// Exercise 3
// Relative Difficulty: 2
// Correctness: 2.5 marks
// Performance: 1 mark
// Elegance: 0.5 marks
// Total: 4
def length[A](as: MList[A]): Int ={
  
  MList.foldLeft(  as, 0,   (x: Int,y : A) =>  x  + 1 )
}
 
// Exercise 4
// Relative Difficulty: 5
// Correctness: 4.5 marks
// Performance: 1.0 mark
// Elegance: 1.5 marks
// Total: 7
def map[A, B](as: MList[A], f: A => B): MList[B] = {
		as match {
		  case Cons(h , t) => Cons(f(h),map(t,f))
		  case Empty => Empty
		}
}
 
// Exercise 5
// Relative Difficulty: 5
// Correctness: 4.5 marks
// Performance: 1.5 marks
// Elegance: 1 mark
// Total: 7
def filter[A](as: MList[A], f: A => Boolean): MList[A] = {
  as match {
    case Cons(h,t) => if (f(h)) Cons(h,filter(t,f))  else   filter(t,f)
    case Empty => Empty
  }
}
 
// Exercise 6
// Relative Difficulty: 5
// Correctness: 4.5 marks
// Performance: 1.5 marks
// Elegance: 1 mark
// Total: 7
def append[A](x: MList[A], y: MList[A]): MList[A] = {
  x match {
    case Cons(h,t) =>    Cons( h, append(t,y)  )
    case Empty => y
  }
}
// Exercise 7
// Relative Difficulty: 5
// Correctness: 4.5 marks
// Performance: 1.5 marks
// Elegance: 1 mark
// Total: 7
def flatten[A](as: MList[MList[A]]): MList[A] = {
  as match {
    case Empty => Empty
    case Cons(h,t) => append ( h, flatten(t))
  }
}
 
// Exercise 8
// Relative Difficulty: 7
// Correctness: 5.0 marks
// Performance: 1.5 marks
// Elegance: 1.5 mark
// Total: 8
def flatMap[A, B](as: MList[A], f: A => MList[B]): MList[B] = {
    flatten (map(as, f))
}
 
// Exercise 9
// Relative Difficulty: 8
// Correctness: 3.5 marks
// Performance: 3.0 marks
// Elegance: 2.5 marks
// Total: 9
def maximum(is: MList[Int]): Int = {
  MList.foldLeft(is,0,(x:Int, y:Int) => if (x>y) x else y  )
}
 
// Exercise 10
// Relative Difficulty: 10
// Correctness: 5.0 marks
// Performance: 2.5 marks
// Elegance: 2.5 marks
// Total: 10
def reverse[A](as: MList[A]): MList[A] = {
  as match {
    case Empty => Empty
    case Cons(h,t) => append ( reverse(t) ,Cons(h,Empty))
  }
}
}







trait PartialType[T[_, _], A] {
  type Apply[B] = T[A, B]
  type Flip[B] = T[B, A]
}

trait Fluffy[F[_]] {
  def furry[A, B](f: A => B, fa: F[A]): F[B]
} 

object Fluffy {
  // Exercise 1
  // Relative Difficulty: 1
  def ListFluffy: Fluffy[List] =  new Fluffy[List] {
    override def furry[A,B]( f: A => B,   fa :List[A] ) :List[B]  = {
      fa.map(f)
    }
  }

  // Exercise 2
  // Relative Difficulty: 1
  def OptionFluffy: Fluffy[Option] = new Fluffy[Option] {
    override def furry[A,B]( f: A => B,   fa :Option[A] ) :Option[B]  = {
      fa.map(f)
      }
    }

  // Exercise 3
  // Relative Difficulty: 1
  def StreamFluffy: Fluffy[Stream] = new Fluffy[Stream] {
    override def furry[A,B] (f : A=>B, fa: Stream[A]  ) : Stream[B] = {
      fa.map(f)
    }
      
    
  }

  // Exercise 4
  // Relative Difficulty: 1
  def ArrayFluffy: Fluffy[Array] = new Fluffy[Array] {
    override def furry[A,B] (f : A=>B, fa: Array[A]  ) : Array[B] = {
      //fa.map(f)
      null
    }
      
    
  }
  
  
//trait PartialType[T[_, _], A] {
//  type Apply[B] = T[A, B]
//  type Flip[B] = T[B, A]
//}

//trait Fluffy[F[_]] {
//  def furry[A, B](f: A => B, fa: F[A]): F[B]
//}
  

  // Exercise 5
  // Relative Difficulty: 5
  def Function1Fluffy[X]: Fluffy[PartialType[Function1, X]#Apply]= new Fluffy[PartialType[Function1, X]#Apply] {
    override def furry[A,B] (f : A=> B, fa: PartialType[Function1, X]#Apply[A] ) : PartialType[Function1, X]#Apply[B]= {
     f.compose(fa)
    }
  }
    
  
    
  

  // Exercise 6
  // Relative Difficulty: 6
  def EitherLeftFluffy[X]: Fluffy[PartialType[Either.LeftProjection, X]#Flip] = new  Fluffy[PartialType[Either.LeftProjection, X]#Flip] {
    override def furry[A,B] (f : A=> B  , fa : PartialType[Either.LeftProjection, X]#Flip[A] ) : PartialType[Either.LeftProjection, X]#Flip[B] = {
        fa.map(f).left
    }
  }
   

  // Exercise 7
  // Relative Difficulty: 4
  def EitherRightFluffy[X]: Fluffy[PartialType[Either.RightProjection, X]#Apply] = new  Fluffy[PartialType[Either.RightProjection, X]#Apply] {
    override def furry[A,B] (f : A=>B, fa:PartialType[Either.RightProjection, X]#Apply[A] ) : PartialType[Either.RightProjection, X]#Apply[B] = {
      fa.map(f).right
    }
    
}
}

trait Misty[M[_]] extends Fluffy[M] {
  def banana[A, B](f: A => M[B], ma: M[A]): M[B]

  def unicorn[A](a: A): M[A]

  // Exercise 8
  // Relative Difficulty: 3
  // (use banana and/or unicorn)
  def furry[A, B](f: A => B, ma: M[A]) = error("todo")
}

object Misty {
  // Exercise 9
  // Relative Difficulty: 2
  def ListMisty: Misty[List] = error("todo")

  // Exercise 10
  // Relative Difficulty: 2
  def OptionMisty: Misty[Option] = error("todo")

  // Exercise 11
  // Relative Difficulty: 2
  def StreamMisty: Misty[Stream] = error("todo")

  // Exercise 12
  // Relative Difficulty: 2
  def ArrayMisty: Misty[Array] = error("todo")

  // Exercise 13
  // Relative Difficulty: 6
  def Function1Misty[X]: Misty[PartialType[Function1, X]#Apply] =
    error("todo")

  // Exercise 14
  // Relative Difficulty: 7
  def EitherLeftMisty[X]: Misty[PartialType[Either.LeftProjection, X]#Flip] =
    error("todo")

  // Exercise 15
  // Relative Difficulty: 5
  def EitherRightMisty[X]: Misty[PartialType[Either.RightProjection, X]#Apply] =
    error("todo")

  // Exercise 16
  // Relative Difficulty: 3
  def jellybean[M[_], A](ma: M[M[A]], m: Misty[M]): M[A] = error("todo")

  // Exercise 17
  // Relative Difficulty: 6
  def apple[M[_], A, B](ma: M[A], mf: M[A => B], m: Misty[M]): M[B] =
    error("todo")

  // Exercise 18
  // Relative Difficulty: 6
  def moppy[M[_], A, B](as: List[A], f: A => M[B], m: Misty[M]): M[List[B]] =
    error("todo")
}

object AdvancedFun {
  case class State[S, A](f: S => (S, A))

  // Exercise 19
  // Relative Difficulty: 9
  def StateFluffy[S]: Fluffy[PartialType[State, S]#Apply] = error("todo")

  // Exercise 20
  // Relative Difficulty: 10
  def StateMisty[S]: Misty[PartialType[State, S]#Apply] = error("todo")
}

