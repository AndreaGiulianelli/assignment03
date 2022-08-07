package model
import monocle.syntax.all.*

case class Body(id: Int, pos: P2d, acc: V2d, vel: V2d, mass: Double):
  import model.Body.frictionConst
  def accelerate(force: V2d): Body = this.focus(_.acc).replace(force / mass)
  def updateVelocity(dt: Double): Body = this.focus(_.vel).modify(_ + acc * dt)
  def updatePos(dt: Double): Body = this.focus(_.pos).modify(_ + vel * dt)
  def currentFrictionForce: V2d = vel * (-frictionConst)
  def checkAndSolveBoundaryCollision(bounds: Boundary): Body =
    (for
      body <- Some(this)
      xUpdated <- Some(body.checkBoundsX(bounds))
      updated <- Some(xUpdated.checkBoundsY(bounds))
    yield updated).get

  private def checkBoundsX(bounds: Boundary): Body = pos match
    case pos if pos.x > bounds.x1 =>
      this.focus(_.pos).replace(P2d(bounds.x1, pos.y)).focus(_.vel).replace(V2d(-vel.x, vel.y))
    case pos if pos.x < bounds.x0 =>
      this.focus(_.pos).replace(P2d(bounds.x0, pos.y)).focus(_.vel).replace(V2d(-vel.x, vel.y))
    case _ => this
  private def checkBoundsY(bounds: Boundary): Body = pos match
    case pos if pos.y > bounds.y1 =>
      this.focus(_.pos).replace(P2d(pos.x, bounds.y1)).focus(_.vel).replace(V2d(vel.x, -vel.y))
    case pos if pos.y < bounds.y0 =>
      this.focus(_.pos).replace(P2d(pos.x, bounds.y0)).focus(_.vel).replace(V2d(vel.x, -vel.y))
    case _ => this

object Body:
  private val repulsiveConst = 0.01
  private val frictionConst = 1
  extension (b: Body)
    def distanceFrom(pos: P2d): Double =
      val dx = b.pos.x - pos.x
      val dy = b.pos.y - pos.y
      Math.sqrt(dx * dx + dy * dy)
    def repulsiveForceBy(pos: P2d, mass: Double): Option[V2d] = b.distanceFrom(pos) match
      case d if d > 0 => V2d(pos, b.pos).norm().map(_ * mass * repulsiveConst / (d * d))
      case _ => None
