class ScalaClass {
  
 import java.awt.{Color, Graphics}
 
abstract class Shape {
  var fillColor:Color = null
 
  def draw(g:Graphics):Unit
  def area:Double
}
  
class Circle(var radius:Int) extends Shape {
  def draw(g:Graphics):Unit = {
    if (fillColor == null) {
      g.drawOval(0, 0, radius / 2, radius / 2)
    } else {
      g.setColor(fillColor);
      g.fillOval(0, 0, radius / 2, radius / 2)
    }
  }
 
  def area:Double = {
    var back = Math.Pi * radius 
    back * radius
  }
}
 
class Square(var width:Int) extends Shape {
  def draw(g:Graphics):Unit = {
    if (fillColor == null) {
      g.drawRect(0, 0, width, width)
    } else {
      g.setColor(fillColor)
      g.fillRect(0, 0, width, width)
    }
  }
 
  def area:Double = width * width
}
    








	

case class Number(value:Int)
 
def checkPrime(n:Number) = n match {
  case Number(1) => true
  case Number(2) => true
  case Number(3) => true
  case Number(5) => true
  case Number(7) => true
  case Number(_) => false
}



















}