import scala.util.Random
import scala.annotation.tailrec
 
class MyList[+A] {
  def head: A = this match {
    case MyListNil => error("head of empty list")
    case x :: xs => x
  }
  def tail: MyList[A] = this match {
    case MyListNil => error("tail of empty list")
    case x :: xs => xs
  }
 
  def ::[B >: A](item: B): MyList[B] = new ::(item, this)
  def isEmpty: Boolean = this match {
    case MyListNil => true
    case x :: xs => false
  }
 
    final def tailRecLength: Int =  {

    @tailrec def lengthIter(iterationsDone : Int ,   rest : MyList[A]) : Int = {
  if (rest == MyListNil)  iterationsDone
  else  lengthIter( iterationsDone + 1 , rest.tail)
}
    
    
    lengthIter(0,this)
}


    def filter(p: A => Boolean): MyList[A] = this match {
case MyListNil => this
case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
}
    
//def forall(p: A => Boolean): Boolean =
//isEmpty || (p(head) && (tail forall p))

    


  


//def exists(p: A => Boolean): Boolean =
//!isEmpty && (p(head) || (tail exists p))


}  //end class

final case class ::[A]( var hd: A,  var tl: MyList[A]) extends MyList[A] {
  override def head: A = hd
  override def tail: MyList[A] = tl
}
 
case object MyListNil extends MyList[Nothing]
 

 
object MyList {
  def apply[A](items: A*): MyList[A] = {
    var list: MyList[A] = MyListNil.asInstanceOf[MyList[A]]
    for (idx <- 0 until items.length reverse)
      list = items(idx) :: list
    list
  }
}











    object HelloWorld {
      
      
      
def forall[A](p: A => Boolean)(xs: MyList[A]): Boolean =
  xs.filter(p).tailRecLength == xs.tailRecLength
  
  
def exists[A](p: A => Boolean)(xs: MyList[A]) : Boolean = 
  xs.filter(p).tailRecLength >= 1
  
  
    
      def main (args : Array[String]) : Unit = { 
  
 
    
    
     

    def f(x: Int): Int = math.pow(x, 2).toInt

 
  
 
}
      
}


    object scalaProblems  {
      
    
      def main (args : Array[String]) : Unit = { 
        
      
           
        
      }
      
        
         
         
         
       def compress(l : List[String]) : List[String] = {
    		  l.foldRight( List[String]())((x : String,y:List[String]) =>  if (y.isEmpty || x!=y.head) x::y  else y)
       }
         
      def pack(l : List[String]) : List[List[String]] = {
    		  l.foldLeft( List[List[String]]())    ((x : List[List[String]],y:String) => if (x.isEmpty || x.head.head !=y  )  List(y)::x else   ( y:: x.head   ):: x.tail    ).reverse
       }

      
    
      def encode(l : List[String]) : List[(Int,String)] = {
        val packed = pack(l)
        packed.map( x => x.length).zip(packed.map(a=>a.head))
      }

      
      def modifiedEncode(ls: List[String]): List[Any] =
    pack(ls).map { e => if (e.length==1) e.head else (e.length, e.head) }
      
      
      def dupl(n: Int, s: String, l :List[String] ) : List[String] = {
        if (n==0) List[String]()
        else s::dupl(n-1,s, List[String]())
      }
       
      
      def decode(l : List[(Int, String)] ) : List[String] = {
        l.flatMap(
           x => dupl(x._1, x._2,List[String]())
            )
      }
      
      
      
         def encodeDirect(l : List[String]) : List[(Int,String)] = {
     null //  l.foldLeft( List[List[String]]())((x : List[List[String]],y:String) => if (x.isEmpty || x.head.head !=y  )  List(y)::x else   ( y:: x.head   ):: x.tail    )
      }
         
         def duplicateN( num : Int, l : List[String]) : List[String] = {
        l.flatMap(  x =>   dupl(num, x , List[String]()))
           
         }
         
         
     def split(num :Int, l : List[String]) : List[String] = {
       
       return null
     }
           
}

    
    
    
    
    
    
    
    trait Observed[E] {
      var observers =  List[Observer[E]]()
	  def addObserver(o: Observer[E]) = {
	   observers =  observers ::: List(o)
	  } 
	  
	  def notifyObservers(ev: E) = {
	    observers.foreach( x=> x.eventOccurred(ev))
	    
	      }
	  }
    
    
    
    
    
    
    
    
	 
	trait Observer[E] {
	  def eventOccurred(ev: E) = {
	    
	  }
	}
    
    
    
    
    
    
    case class Book(var title: String, var price: Double) extends Observed[java.beans.PropertyChangeEvent] {
	  def setTitle(title: String) {
	    val oldTitle = this.title
	    this.title = title
	   notifyObservers( new java.beans.PropertyChangeEvent(this, "title", oldTitle, title) )
	  }
	  def setPrice(price: Double) {
	    val oldPrice = this.price
	    this.price = price
	   notifyObservers( new java.beans.PropertyChangeEvent(this, "price", oldPrice, price) )
	  }
	}
	 
	class Foo extends Observer[java.beans.PropertyChangeEvent] {
	  override def eventOccurred(ev: java.beans.PropertyChangeEvent) = {
	    printf("Foo: %s of %s has changed from %s to %s\n", ev.getPropertyName, ev.getSource, ev.getOldValue, ev.getNewValue)
	  }
	}
	 
	object BeanTest {
	  def main(args: Array[String]) {
	    println("blah")
	    val b1 = new Book("Scala programming", 35.95)
	    val foo = new Foo
	    b1.addObserver(foo) //Register the observer
	    b1.setTitle("Thinking in Scala")  //foo should get an event
	    b1.setPrice(39.95) //ditto
	    b1.setTitle("Effective Scala") //ditto
	  }
	}
    
    
    
    
    
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
    trait Callback {
  def onCallback(ev: Any)
}
    
    trait ErrorHandler{
       def handle(e: Exception): Boolean
	 
	  def plus(fallback: ErrorHandler): ErrorHandler =
	    new ErrorHandler() {
	      def handle(e: Exception) =
	        ErrorHandler.this.handle(e) || fallback.handle(e)
	    }
    }

abstract class ErrorHandlingCallback(errorHandler: ErrorHandler) extends Callback {
	  def this(errorHandlers: ErrorHandler*) =
	    this (errorHandlers.reduceRight(_ plus _))

  def performBusinessLogic(ev: Any)

  def onCallback(ev: Any) {
    try {
    performBusinessLogic(ev) 
    }
    catch { 
      case e : Exception => {
        if (!errorHandler.handle(e)) {
          throw e
        }
        
      }
     
   }
      
    
  }
}

object ErrorHandlerUtil {
  implicit def fromFunc(f: Exception => Boolean): ErrorHandler =
	    new ErrorHandler() {
	      def handle(e: Exception) = f(e)
	    }
  implicit def fromPair(p: (Class[_ <: Exception], String)): ErrorHandler =
	    new ClassBasedErrorReporter(p._1, p._2)
}



	class ClassBasedErrorReporter(
	        //the type means Class[E] where E is a subclass of Exception
	        errorClass: Class[_ <: Exception],
	        msg: String) extends ErrorHandler {
	  def handle(e: Exception) = {
	    if (errorClass.isInstance(e)) {
	      println(msg)
	      true
	    } else {
	      false
	    }
	  }
	}

object ErrorHandlerTest {

  import ErrorHandlerUtil._

  def main(args: Array[String]) {
    //assume that this is the default error handler in this context
    //classOf[Foo] is the same as Foo.class in Java
    // val defaultErrorHandler = null
    val defaultErrorHandler = (classOf[java.io.IOException], "I/O error") plus (classOf[Exception], "Unknown catch all error")
    //create a decorator to handle additional errors
    val decorator = new ErrorHandlingCallback(
      //convert a pair to an error handler
      (classOf[IndexOutOfBoundsException], "index out of bound"),
      //ditto
      (classOf[NullPointerException], "hit a null pointer"),
      //you can define a custom error handler using a function to, say,
      //access the info in the exception (not just its class).
      (e: Exception) => if (e.getMessage.contains("xyz")) {
        println(e.getMessage)
        true //indicate that it has been handled
      } else false,
      //specify the default error handler here
      defaultErrorHandler) {
      def performBusinessLogic(ev: Any) {
        println("called")
        ev match {
          //do nothing. No error.
          case "foo" =>
          //try to access the 100th element of an array which has only 3 elements
          case "bar" => Array[Int](1, 2, 3).apply(100)
          //Try to call a method on null
          case "baz" => null.equals("oops!")
          //throw a custom exception
          case "baz2" => throw new RuntimeException("I am xyz!")
          //divided by zero (something unexpected to test the ultimate fallback)
          case "baz3" => 100 / 0
        }
      }
    }
    decorator.onCallback("foo")
    decorator.onCallback("bar")
    decorator.onCallback("baz")
    decorator.onCallback("baz2")
    decorator.onCallback("baz3")
  }
}



















import java.net.URL
	import io.Source
	 
	object HttpClient extends CircuitBreaker[Unit] {
	  def main(args: Array[String]) {
	    while (true) {
	      print("Enter: ")
	      try {
	        readLine match {
	          case "q" => queryHttpServer
	          case "r" => reset
	          case "x" => {
	            println("Exiting")
	            return
	          }
	          case _ => println("Unknown command")
	        }
      } catch {
	        case e: Exception => e.printStackTrace
	      }
	    }
	  }
	 
	  def queryHttpServer {
	    protect {
      //a Source object is a like a sequence of char
	      val s = Source.fromURL(new URL("http://localhost:1234"))
	      s.foreach(ch => print(ch))
	      s.close
	    }
	  }
	}
	 
	trait CircuitBreaker[T] {
	  var isOpen = false
	 
	  //the => in the type for the codeBlock argument means that the codeBlock parameter
	  //will be passed by name. That is, it will not be evaluated when passed. Instead,
	  //whenever you refer to codeBlock in the method body, it will be evaluated on demand.
	  def protect(codeBlock: => T): T = {
	    if (isOpen) {
	      println("Circuit is open, not calling the real thing")
	      throw new CircuitOpenException
	    } else {
	      try {
	        codeBlock //evaluate the code block on demand
	      } catch {
	        case e: Exception => {
	          println("Got an exception, opening the circuit")
	          isOpen = true
	          throw e
	        }
	      }
	    }
	  }
	 
	  def reset {
	    println("Resetting the circuit")
	    isOpen = false
	  }
	}
	 
	class CircuitOpenException extends Exception






















    
    
    
    
    

