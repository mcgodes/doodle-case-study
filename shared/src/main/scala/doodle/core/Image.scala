package doodle
package core

import doodle.backend.Canvas

sealed trait Image extends Product with Serializable {
  def on(other: Image): Image = CompoundImage(this, other)
  def above(other: Image) = ???
  def below(other: Image): Image = ???
}

final case class CompoundImage(top: Image, others: Image) extends Image
final case class Rectangle(width: Double, height: Double, origin: Point = Origin) extends Image
final case class Circle(radius: Double, origin: Point = Origin) extends Image
final case class Triangle(base: Double, height: Double, origin: Point = Origin) extends Image

case class Point(x: Double, y: Double)
object Origin extends Point(0, 0)

object Draw {
  def apply(canvas: Canvas, image: Image): Unit = {
    image match {
      case r: Rectangle =>
        canvas.rectangle(r.origin.x, r.origin.y, r.width, r.height)

      case c: Circle =>
        canvas.circle(c.origin.x, c.origin.y, c.radius)

      case t: Triangle =>

      case ci: CompoundImage =>
        Draw(canvas, ci.others)
        Draw(canvas, ci.top)
    }

    canvas.setStroke(Stroke(1.0, Color.black, Line.Cap.Round, Line.Join.Round))
    canvas.stroke()
  }
}

object DoDraw {
  def apply(canvas: Canvas): Unit = {
    canvas.setSize(1000, 1000)
    val c = Circle(100)
    val t = Circle(30)
    Draw(canvas, c on t)
  }
}
