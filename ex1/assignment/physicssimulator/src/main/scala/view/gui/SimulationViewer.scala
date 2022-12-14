package view.gui

import akka.actor.typed.ActorRef
import controller.Coordinator
import model.{Boundary, P2d}
import java.awt.event.{ActionEvent, WindowAdapter, WindowEvent}
import java.awt.BorderLayout
import java.util
import java.util.List
import javax.swing.{JButton, JFrame, JPanel, SwingUtilities}

trait SimulationViewer:
  /** Display the view */
  def display(): Unit
  /** Update the view
    * @param positions
    *   list of the bodies position
    * @param vt
    *   virtual time
    * @param iter
    *   iteration counter
    * @param bounds
    *   boundary of the environment
    */
  def update(positions: Seq[P2d], vt: Double, iter: Long, bounds: Boundary): Unit
  /** Method to inform the view that the simulation is ended */
  def simulationEnd(): Unit

object SimulationViewer:
  def apply(w: Int, h: Int, coordinator: ActorRef[Coordinator.Command]): SimulationViewer =
    SimulationViewerImpl(w, h, coordinator)
  private class SimulationViewerImpl(w: Int, h: Int, coordinator: ActorRef[Coordinator.Command])
      extends JFrame
      with SimulationViewer:
    private val panel = VisualiserPanel(w, h)
    private val start = JButton("Start")
    private val stop = JButton("Stop")

    private val buttonsPanel = new JPanel
    private val mainPanel = new JPanel
    private val mainLayout = new BorderLayout

    override def display(): Unit =
      SwingUtilities.invokeLater { () =>
        setTitle("Bodies Simulation")
        setSize(w, h + 40)
        setResizable(false)
        buttonsPanel.add(start)
        buttonsPanel.add(stop)

        mainPanel.setLayout(mainLayout)
        mainPanel.add(BorderLayout.CENTER, panel)
        mainPanel.add(BorderLayout.SOUTH, buttonsPanel)

        setContentPane(mainPanel)

        start.setEnabled(true)
        start.addActionListener { _ =>
          start.setEnabled(false)
          if start.getText == "Resume" then coordinator ! Coordinator.Command.Resume
          else
            coordinator ! Coordinator.Command.Start
            start.setText("Resume")
          stop.setEnabled(true)
        }

        stop.setEnabled(false)
        stop.addActionListener { _ =>
          coordinator ! Coordinator.Command.Stop
          start.setEnabled(true)
          stop.setEnabled(false)
        }
        addWindowListener(new WindowAdapter():
          override def windowClosing(ev: WindowEvent): Unit =
            System.exit(-1)

          override def windowClosed(ev: WindowEvent): Unit =
            System.exit(-1)
        )
        setVisible(true)
      }

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
