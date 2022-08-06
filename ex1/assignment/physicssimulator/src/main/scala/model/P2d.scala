package model

case class P2d(x: Double, y: Double)

object P2d:
  extension (p: P2d) def +(other: V2d): P2d = P2d(p.x + other.x, p.y + other.y)
