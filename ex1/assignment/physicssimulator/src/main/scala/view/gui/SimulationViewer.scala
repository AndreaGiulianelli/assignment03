package view.gui

import akka.actor.typed.ActorRef
import controller.Coordinator
import model.{Boundary, P2d}
import java.awt.event.{ActionEvent, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, LayoutManager}
import java.util
import java.util.List
import javax.swing.{JButton, JFrame, JPanel, SwingUtilities}

trait SimulationViewer:
  def display(): Unit
  def update(bodies: Seq[P2d], vt: Double, iter: Long, bounds: Boundary): Unit
  def simulationEnd(): Unit

object SimulationViewer:
  def apply(w: Int, h: Int, coordinator: ActorRef[Coordinator.Command]): SimulationViewer =
    SimulationViewerImpl(w, h, coordinator)
  private class SimulationViewerImpl(w: Int, h: Int, coordinator: ActorRef[Coordinator.Command])
      extends JFrame
      with SimulationViewer:
    val panel = VisualiserPanel(w, h)
    val start = JButton("Start")
    val stop = JButton("Stop")

    setTitle("Bodies Simulation")
    setSize(w, h + 40)
    setResizable(false)
    val buttonsPanel = new JPanel
    buttonsPanel.add(start)
    buttonsPanel.add(stop)

    val mainPanel = new JPanel
    val mainLayout = new BorderLayout
    mainPanel.setLayout(mainLayout)
    mainPanel.add(BorderLayout.CENTER, panel)
    mainPanel.add(BorderLayout.SOUTH, buttonsPanel)

    setContentPane(mainPanel)

    start.setEnabled(true)
    start.addActionListener { (e: ActionEvent) =>
      coordinator ! Coordinator.Command.Start
      start.setEnabled(false)
      start.setText("Resume")
      stop.setEnabled(true)
    }

    stop.setEnabled(false)
    stop.addActionListener { (e: ActionEvent) =>
      coordinator ! Coordinator.Command.Stop
      start.setEnabled(true)
      stop.setEnabled(false)
    }
    this.addWindowListener(new WindowAdapter():
      override def windowClosing(ev: WindowEvent): Unit =
        System.exit(-1)
      override def windowClosed(ev: WindowEvent): Unit =
        System.exit(-1)
    )

    override def display(): Unit =
      SwingUtilities.invokeLater(() => setVisible(true))

    def update(positions: Seq[P2d], vt: Double, iter: Long, boundary: Boundary): Unit =
      SwingUtilities.invokeLater { () =>
        panel.display(positions, vt, iter, boundary)
        repaint()
      }

    override def simulationEnd(): Unit =
      SwingUtilities.invokeLater { () =>
        start.setEnabled(false)
        stop.setEnabled(false)
      }
