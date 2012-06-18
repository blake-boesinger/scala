object blahblah { 
 
 
abstract class SemiGroup[A] { 
def add(x: A, y: A): A 
}
 
 
 
abstract class Monoid[A] extends SemiGroup[A] { 
def unit: A 
}
 
 
implicit object stringMonoid extends Monoid[String] { 
def add(x: String, y: String): String = x.concat(y) 
def unit: String = "" 
}
 
 
implicit object intMonoid extends Monoid[Int] { 
def add(x: Int, y: Int): Int = x + y 
def unit: Int = 0 
}
 
 
 
  
 
        
     
      def main (args : Array[String]) : Unit = { 
  
  
 
  
  
 
}
 
 
import scala.Left; 
import scala.annotation.tailrec 
def sum(a: Int, b: Int): Int = { 
  @tailrec def iter(a : Int, result : Int) : Int = { 
if (a<=0) result 
else   
  iter(a-1, result+a)
}
iter(b-a, b)
}
 
 
 
def sumOrProduct(f: Int => Int)(a: Int, b: Int)(op: (Int,Int) => Int)  (base: Int) : Int =  { 
  
  if (a>b) base 
  else       op(   f(a) ,  sumOrProduct(f)(a + 1, b)(op: (Int,Int) => Int)  (base: Int)  ) 
}
 
 
def sum(f: Int => Int)(a: Int, b: Int): Int = 
if (a > b) 0 
else f(a) + sum(f)(a + 1, b) 
 
 
 
def product(f: Int => Int)(a: Int, b: Int): Int = { 
if (a > b) 1 
else   
  f(a) * product(f)(a + 1, b) 
     
}
 
 
def fac(n : Int) : Int =   { 
  product ( x => x) (1,n)
}
 
 




 
 
 
 
 
trait IntSet { 
def incl(x: Int): IntSet 
def contains(x: Int): Boolean 
def isEmpty() : Boolean 
def union(other : IntSet): IntSet 
def intersect(other : IntSet): IntSet 
override def equals(other: Any): Boolean 
def excl(x: Int) : IntSet 
}
 
 
 
 
 
 
class EmptySet extends IntSet { 
def contains(x: Int): Boolean = false 
def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet) 
override def toString = "_," 
  override def equals(other: Any) = { 
    other.isInstanceOf[EmptySet]
  }
def isEmpty() : Boolean = { 
  return true 
}
 
def excl(x : Int): IntSet = this 
 
 
 
def union( other : IntSet) :IntSet = { 
  return other 
}
def intersect( other : IntSet) :IntSet = { 
  return this 
}
 
def descend( other : IntSet) :IntSet = { 
  return this 
}
 
}
 
 
 
class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet { 
  
  
  override def excl( x: Int) : IntSet = { 
    if (x < elem )  new NonEmptySet(elem, left excl x, right) 
    else if (x > elem) new NonEmptySet(elem, left, right excl x) 
    else left.union(right) 
  }
  
  override def equals(other: Any) = { 
    other match { 
      case that: NonEmptySet => 
        that.toString == toString
      case _ => false 
    }
  }
  
   override def toString = 
    left.toString + elem + "," + right.toString 
  
  def isEmpty() : Boolean = { 
    return false ; 
  }
  
def contains(x: Int): Boolean = 
if (x < elem) left contains x 
else if (x > elem) right contains x 
else true 
 
 
 
def incl(x: Int): IntSet = 
if (x < elem) new NonEmptySet(elem, left incl x, right) 
else if (x > elem) new NonEmptySet(elem, left, right incl x) 
else this 
 
 
 
 
 
 
def union( other : IntSet) : IntSet = { 
  
  return left.union( right.union(other)  ).incl(elem) 
   
}
 
 
 
 
 
 
def intersect(other : IntSet): IntSet = { 
 
  val rightLeft =  right.intersect(other).union(left.intersect(other)) 
 
  if (other.contains(elem)) rightLeft.incl(elem) else rightLeft 
   
}
 
 
 
 
 
 
 
 
      
}
 
}
 
 
 
 
 
 
 
 
 
 
 
 
 
 
object Test { 
  val nonEmptySet0 = definedNoneEmptySet 
  val nonEmptySet1 = variableNoneEmptySet(6, 5, -2) 
  val emptySet = new blahblah.EmptySet 
 
  def main(args: Array[String]) { 
    testToString
    testEquals
 
    testAppend
    testUnion
    testIntersect
 
    testIsEmpty
    testExcl
  }
 
  def testExcl = { 
    assert(emptySet.excl(2) == emptySet)
    assert(nonEmptySet1.excl(6) == variableNoneEmptySet(5, -2))
    assert(nonEmptySet1.excl(99) == nonEmptySet1)
  }
 
  def testIsEmpty = { 
    assert(!nonEmptySet0.isEmpty)
    assert(emptySet.isEmpty)
  }
 
  def testIntersect = { 
    assert(emptySet.intersect(emptySet) == emptySet)
    assert(emptySet.intersect(nonEmptySet0) == emptySet)
    assert(nonEmptySet0.intersect(emptySet) == emptySet)
    assert(nonEmptySet0.intersect(nonEmptySet0) == nonEmptySet0)
    assert(nonEmptySet0.intersect(nonEmptySet1).toString == "_,-2,_," ) 
  }
 
  def testUnion = { 
    assert(emptySet.union(nonEmptySet0) == nonEmptySet0)
    assert(nonEmptySet0.union(nonEmptySet0) == nonEmptySet0)
    assert(nonEmptySet0.union(nonEmptySet1) != nonEmptySet0)
    assert(nonEmptySet0.union(nonEmptySet1).toString == "_,-2,_,1,_,2,_,5,_,6,_," ) 
  }
 
  def testAppend = { 
  //   assert(emptySet.append(nonEmptySet0) == nonEmptySet0) 
  //   assert(nonEmptySet0.append(nonEmptySet0) == nonEmptySet0) 
  //  assert(nonEmptySet0.append(nonEmptySet1) != nonEmptySet0) 
  //  assert(nonEmptySet0.append(nonEmptySet1).toString == "_,-2,_,1,_,2,_,5,_,6,_,") 
  }
 
  def testToString = { 
    assert(definedNoneEmptySet.toString == "_,-2,_,1,_,2,_," ) 
  }
 
  def testEquals = { 
    val set0 = definedNoneEmptySet 
    val set1 = definedNoneEmptySet 
    assert(set0 == set1)
 
    val set2 = variableNoneEmptySet(3, 4, 5) 
    assert(set0 != set2)
  }
 
  def variableNoneEmptySet(elems: Int*): blahblah.IntSet = { 
    var set: blahblah.IntSet = new blahblah.NonEmptySet(elems(0), new blahblah.EmptySet, new blahblah.EmptySet) 
 
    for (elem <- elems) 
      set = set.incl(elem)
 
    set
  }
 
  def definedNoneEmptySet: blahblah.IntSet = { 
    variableNoneEmptySet(1, 2, -2)
  }
}
 
 
 
 
 
 
 object blah extends App {
val employees: List[Option[String]] =
  List(Some("dave"), None, Some("john"), Some("sam"))


val n: Int =
  employees.map { x =>
    x match {
      case Some(name) => name.length
      case None => 0
    }
  }.elements.reduceLeft[Int](_ + _)
println(n)




val brs: List[List[String]] =
  List(List("dave", "john", "sam"), List("peter", "robin", "david"), List("pras", "srim"))
  
  
  val m: Int =
  brs.flatMap {x => x.map {_.length}}
     .elements.reduceLeft[Int](_ + _)
println(m)







 }
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

object Board {

	type Grid = Array[Array[Cell]]

	def transform(s:String) : String = {

    val grid = parse(s)

    val transformedGrid = updateNeighbours(grid);

    val slots: Array[String] = for {row <- grid } yield codeFor(row)

    slots reduceLeft({_+'\n'+_})


	}
  def updateNeighbours(grid: Grid): Grid = {

    //val newGrid = new scala.collection.mutable.ArrayList[new scala.collection.mutable.ArrayList]()

    for(row <- (0 to grid.length -1)) {
      for (col <- (0 to grid(row).length -1)) {
        val minesNearby = getNeighbourCells(grid, row, col).filter(_.isMine).length
        grid(row)(col) = Cell(grid(row)(col).isMine, if (grid(row)(col).isMine)  None else Some(minesNearby))

      }
    }
    grid
  }


  def getNeighbourCells(grid: Grid,
                        row: Int,
                        col: Int): Seq[Cell] = {
    val offsets = List(-1, 0, 1)
    for (rowOffset <- offsets; colOffset <- offsets
         if rowOffset != 0 || colOffset != 0
      if rowOffset + row >= 0
      if colOffset + col >= 0
      if rowOffset + row < grid.length
      if colOffset + col < grid(0).length

    )
      yield grid(row + rowOffset)(col + colOffset)
  }


  def processCell(cells : (Cell, Cell, Cell)) = {
     null
  }

  def codeFor(row : Array[Cell]):String = {
    row.map{x => x.isMine match {
      case true => '*'
      case false => x.hint.get.toString
    }}.mkString("")
  }

	def parse(in: String): Grid = {
		def parseRow(row: String) = {
			row.map {
				case '*' => new Cell(true)
				case _   => new Cell(false)
			} toArray
		}
		in.split('\n').map(parseRow _).toArray
	}

}


case class Cell(val isMine: Boolean, val hint: Option[Int] = None) {

}





