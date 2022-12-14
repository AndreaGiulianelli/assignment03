package firestation.gui

import akka.actor.typed.ActorRef
import firestation.Firestation
import model.CityModel.{ALARM, BUSY, FREE, FirestationService, NORMAL, UNDER_MANAGEMENT, Zone}
import java.awt.{Color, Component, Dimension}
import javax.swing.{Box, BoxLayout, JButton, JFrame, JLabel, JPanel, SwingUtilities}

trait FirestationViewer:
  /** Display the dashboard */
  def display(): Unit
  /** Update the dashboard state
    * @param firestation
    *   the updated firestation
    */
  def update(firestation: FirestationService): Unit

object FirestationViewer:
  def apply(zone: Zone, width: Int, height: Int, firestation: ActorRef[Firestation.Command]): FirestationViewer =
    FirestationViewerImpl(zone, width, height, firestation)
  private class FirestationViewerImpl(
      private var zone: Zone,
      width: Int,
      height: Int,
      firestationActor: ActorRef[Firestation.Command]
  ) extends JFrame
      with FirestationViewer:

    private var zoneMap: Map[Int, FirestationService] = Map.empty
    // Current zone status
    private val statusPanel = JPanel()
    private val statusLayout = BoxLayout(statusPanel, BoxLayout.Y_AXIS)
    private val zoneIdLabel = JLabel("Zone id: ")
    private val zoneStatusLabel = JLabel("Zone status: ")
    private val zoneSensorsLabel = JLabel("Zone sensors: ")
    private val firestationStatusLabel = JLabel("Firestation status: ")
    private val managementBtn = JButton("Manage")
    // Other zones status
    private val zonesPanel = JPanel()

    //Main panel
    private val mainPanel = JPanel()
    private val mainLayout = BoxLayout(mainPanel, BoxLayout.Y_AXIS)
    private val otherZoneSectionTitle = JLabel("Other zones:")
    // Maps for text and colors based on status
    private val textZoneMap = Map(NORMAL() -> "Normal", ALARM() -> "Alarm", UNDER_MANAGEMENT() -> "Under management")
    private val textFirestationMap = Map(FREE() -> "Free", BUSY() -> "Busy")
    private val colorZoneMap = Map(NORMAL() -> Color.green, ALARM() -> Color.red, UNDER_MANAGEMENT() -> Color.orange)
    private val colorFirestationMap = Map(FREE() -> Color.green, BUSY() -> Color.red)

    override def display(): Unit = SwingUtilities.invokeLater { () =>
      setTitle(s"Zone#${zone.zoneId}")
      setSize(width, height)

      zoneStatusLabel.setOpaque(true)
      firestationStatusLabel.setOpaque(true)
      statusPanel.setLayout(statusLayout)
      statusPanel.add(zoneIdLabel)
      statusPanel.add(zoneStatusLabel)
      statusPanel.add(zoneSensorsLabel)
      statusPanel.add(firestationStatusLabel)
      statusPanel.add(managementBtn)
      zoneIdLabel.setAlignmentX(Component.LEFT_ALIGNMENT)
      zoneStatusLabel.setAlignmentX(Component.LEFT_ALIGNMENT)

      managementBtn.addActionListener { _ =>
        if zone.status == ALARM() then
          // set under management
          firestationActor ! Firestation.AlarmUnderManagement
          managementBtn.setText("Solve")
        else if zone.status == UNDER_MANAGEMENT() then
          // solve
          firestationActor ! Firestation.AlarmSolved
          managementBtn.setText("Manage")
      }

      mainPanel.setLayout(mainLayout)
      mainPanel.add(statusPanel)
      mainPanel.add(Box.createRigidArea(Dimension(0, 50)))
      mainPanel.add(otherZoneSectionTitle)
      mainPanel.add(zonesPanel)
      statusPanel.setAlignmentX(Component.LEFT_ALIGNMENT)
      zonesPanel.setAlignmentX(Component.LEFT_ALIGNMENT)
      setContentPane(mainPanel)
      setVisible(true)
    }

    override def update(firestation: FirestationService): Unit = SwingUtilities.invokeLater { () =>
      if firestation.associatedZone.zoneId == zone.zoneId then
        zone = firestation.associatedZone
        zoneIdLabel.setText(s"Zone id: ${firestation.associatedZone.zoneId}")
        zoneStatusLabel.setText(s"Zone status: ${textZoneMap(firestation.associatedZone.status)}")
        zoneStatusLabel.setBackground(colorZoneMap(firestation.associatedZone.status))
        zoneSensorsLabel.setText(s"Zone sensors: ${firestation.associatedZone.sensors}")
        firestationStatusLabel.setText(s"Firestation status: ${textFirestationMap(firestation.status)}")
        firestationStatusLabel.setBackground(colorFirestationMap(firestation.status))
      else
        zoneMap = zoneMap + (firestation.associatedZone.zoneId -> firestation)
        render()
    }

    private def render(): Unit =
      zonesPanel.removeAll()
      zonesPanel.revalidate()
      zonesPanel.repaint()
      zoneMap.foreach((_, firestation) => zonesPanel.add(createFirestationRender(firestation)))

    private def createFirestationRender(firestation: FirestationService): JPanel =
      val zoneToDisplay = firestation.associatedZone
      val panel = JPanel()
      val layout = BoxLayout(panel, BoxLayout.Y_AXIS)
      val status = JLabel(
        s"Zone ${zoneToDisplay.zoneId} " +
          s"- sensors: ${zoneToDisplay.sensors} " +
          s"- status: ${textZoneMap(zoneToDisplay.status)} " +
          s"- firestation: ${textFirestationMap(firestation.status)}"
      )
      panel.setLayout(layout)
      status.setBackground(colorZoneMap(zoneToDisplay.status))
      status.setOpaque(true)
      panel.add(status)
      panel
