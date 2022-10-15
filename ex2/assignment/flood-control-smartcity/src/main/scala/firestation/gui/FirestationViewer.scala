package firestation.gui

import akka.actor.typed.ActorRef
import firestation.Firestation
import model.CityModel.Zone
import model.CityModel.ZoneStatus

import java.awt.{Color, Component, Dimension}
import javax.swing.{Box, BoxLayout, JButton, JFrame, JLabel, JPanel, SwingUtilities}

//todo: add all the necessary methods.
trait FirestationViewer:
  def display(): Unit

object FirestationViewer:
  def apply(zone: Zone, width: Int, height: Int, firestation: ActorRef[Firestation.Command]): FirestationViewer =
    FirestationViewerImpl(zone, width, height, firestation)
  private class FirestationViewerImpl(zone: Zone, width: Int, height: Int, firestation: ActorRef[Firestation.Command])
      extends JFrame
      with FirestationViewer:

    private var zoneMap: Map[Int, Zone] = Map(
      0 -> Zone(0, status = ZoneStatus.ALARM),
      1 -> Zone(1),
      2 -> Zone(2),
      3 -> Zone(3, status = ZoneStatus.UNDER_MANAGEMENT),
      4 -> Zone(4)
    )
    setTitle(s"Zone#${zone.zoneId}")
    setSize(width, height)

    // Current zone status
    private val statusPanel = JPanel()
    private val statusLayout = BoxLayout(statusPanel, BoxLayout.Y_AXIS)
    private val zoneIdLabel = JLabel("Zone id: ")
    private val zoneStatusLabel = JLabel("Zone status: ")
    private val managementBtn = JButton("Manage")
    statusPanel.setLayout(statusLayout)
    statusPanel.add(zoneIdLabel)
    statusPanel.add(zoneStatusLabel)
    statusPanel.add(managementBtn)
    zoneIdLabel.setAlignmentX(Component.LEFT_ALIGNMENT)
    zoneStatusLabel.setAlignmentX(Component.LEFT_ALIGNMENT)

    // Other zones status
    private val zonesPanel = JPanel()
    render()

    //Main panel
    private val mainPanel = JPanel()
    private val mainLayout = BoxLayout(mainPanel, BoxLayout.Y_AXIS)
    val zonesTitle = JLabel("Other zones:")
    mainPanel.setLayout(mainLayout)
    mainPanel.add(statusPanel)
    mainPanel.add(Box.createRigidArea(Dimension(0, 50)))
    mainPanel.add(zonesTitle)
    mainPanel.add(zonesPanel)
    statusPanel.setAlignmentX(Component.LEFT_ALIGNMENT)
    zonesPanel.setAlignmentX(Component.LEFT_ALIGNMENT)
    setContentPane(mainPanel)

    override def display(): Unit = SwingUtilities.invokeLater(() => setVisible(true))

    private def render(): Unit =
      zonesPanel.removeAll()
      zoneMap.foreach((_, zoneToDisplay) => zonesPanel.add(createZoneRender(zoneToDisplay)))

    private def createZoneRender(zoneToDisplay: Zone): JPanel =
      val panel = JPanel()
      val layout = BoxLayout(panel, BoxLayout.Y_AXIS)
      val status = JLabel(zoneToDisplay.toString)
      panel.setLayout(layout)
      status.setBackground(zoneToDisplay.status match
        case ZoneStatus.NORMAL => Color.green
        case ZoneStatus.ALARM => Color.red
        case ZoneStatus.UNDER_MANAGEMENT => Color.orange
      )
      status.setOpaque(true)
      panel.add(status)
      panel

object tryGUI extends App:
  val gui = FirestationViewer(0, 400, 400, null)
  gui.display()
