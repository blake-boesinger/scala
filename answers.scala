//trait PartialType[T[_, _], A] {
//  type Apply[B] = T[A, B]
//  type Flip[B] = T[B, A]
//}

//trait Fluffy[F[_]] {
//  def furry[A, B](f: A => B, fa: F[A]): F[B]
//}

object F {

  // Exercise 1
  // Relative Difficulty: 1
  def ListFluffy: Fluffy[List] = new Fluffy[List] {
    def furry[A, B](f: A => B, fa: List[A]): List[B] = fa map f
  }

  // Exercise 2
  // Relative Difficulty: 1
  def OptionFluffy: Fluffy[Option] = new Fluffy[Option] {
    def furry[A, B](f: A => B, fa: Option[A]): Option[B] = fa map f
  }

  // Exercise 3
  // Relative Difficulty: 1
  def StreamFluffy: Fluffy[Stream] = new Fluffy[Stream] {
    def furry[A, B](f: A => B, fa: Stream[A]): Stream[B] = fa map f
  }

  // Exercise 4
  // Relative Difficulty: 1
  def ArrayFluffy: Fluffy[Array] = new Fluffy[Array] {
    def furry[A, B](f: A => B, fa: Array[A]): Array[B] = null//fa map f
  }

  // Exercise 5
  // Relative Difficulty: 5
  def Function1Fluffy[X]: Fluffy[PartialType[Function1, X]#Apply] = new Fluffy[PartialType[Function1, X]#Apply] {
    def furry[A, B](f: A => B, fa: Function1[X, A]) = f compose fa
  }

  // Exercise 6
  // Relative Difficulty: 6
  def EitherLeftFluffy[X]: Fluffy[PartialType[Either.LeftProjection, X]#Flip] = new Fluffy[PartialType[Either.LeftProjection, X]#Flip] {
    def furry[A, B](f: A => B, fa: Either.LeftProjection[A, X]) = fa map f left
  }

  // Exercise 7
  // Relative Difficulty: 4
  def EitherRightFluffy[X]: Fluffy[PartialType[Either.RightProjection, X]#Apply] = new Fluffy[PartialType[Either.RightProjection, X]#Apply] {
    def furry[A, B](f: A => B, fa: Either.RightProjection[X, A]) = fa map f right
  }
}