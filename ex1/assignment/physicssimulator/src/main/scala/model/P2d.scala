package model

case class P2d(x: Double = 0, y: Double = 0)

object P2d:
  extension (p: P2d) def +(other: V2d): P2d = P2d(p.x + other.x, p.y + other.y)
