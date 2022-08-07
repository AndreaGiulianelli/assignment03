package model

case class V2d(x: Double = 0, y: Double = 0)

object V2d:
  def apply(from: P2d, to: P2d): V2d = V2d(to.x - from.x, to.y - from.y)
  extension (v: V2d)
    def *(k: Double): V2d = V2d(v.x * k, v.y * k)
    def +(other: V2d): V2d = V2d(v.x + other.x, v.y + other.y)
    def /(k: Double): V2d = v * (1 / k)
    def norm(): Option[V2d] = Math.sqrt(v.x * v.x + v.y * v.y) match
      case x if x > 0 => Some(v / x)
      case _ => None
