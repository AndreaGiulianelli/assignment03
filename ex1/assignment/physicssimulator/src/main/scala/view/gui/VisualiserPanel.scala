package view.gui

import model.{Boundary, P2d}

import java.awt.{Graphics, Graphics2D, RenderingHints}
import javax.swing.JPanel
import java.awt.event.{KeyEvent, KeyListener}

class VisualiserPanel(w: Int, h: Int) extends JPanel with KeyListener:
  private var bodiesPositions: Seq[P2d] = Seq.empty
  private var boundary: Boundary = Boundary(-4, -4, 4, 4)
  private var nIter: Long = 0
  private var vt: Double = 0.01
  private var scale: Double = 1
  val dx = w / 2
  val dy = h / 2 - 20

  setSize(w, h)
  this.addKeyListener(this)
  setFocusable(true)
  setFocusTraversalKeysEnabled(false)
  requestFocusInWindow

  override def paint(g: Graphics): Unit =
    if bodiesPositions != null then
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2.clearRect(0, 0, this.getWidth, this.getHeight)
      val x0 = getXcoord(boundary.x0)
      val y0 = getYcoord(boundary.y0)
      val wd = getXcoord(boundary.x1) - x0
      val ht = y0 - getYcoord(boundary.y1)
      g2.drawRect(x0, y0 - ht, wd, ht)
      for
        pos <- bodiesPositions
        radius = Math.max((10 * scale).toInt, 1)
      do g2.drawOval(getXcoord(pos.x), getYcoord(pos.y), radius, radius)
      val time = String.format("%.2f", vt)
      g2.drawString(
        "Bodies: " + bodiesPositions.size + " - vt: " + time + " - nIter: " + nIter + " (UP for zoom in, DOWN for zoom out)",
        2,
        20
      )
  private def getXcoord(x: Double): Int = (dx + x * dx * scale).toInt
  private def getYcoord(y: Double): Int = (dy - y * dy * scale).toInt
  def display(bodies: Seq[P2d], vt: Double, iter: Long, bounds: Boundary): Unit =
    this.bodiesPositions = bodies
    this.boundary = bounds
    this.vt = vt
    this.nIter = iter
  def updateScale(k: Double): Unit = scale = scale * k
  override def keyPressed(e: KeyEvent): Unit =
    if e.getKeyCode == 38 then scale = scale * 1.1
    else if e.getKeyCode == 40 then scale = scale * 0.9
  override def keyReleased(e: KeyEvent): Unit = {}
  override def keyTyped(e: KeyEvent): Unit = {}
