package app

import java.awt.Color
import javax.swing.BorderFactory

import scala.swing._
import scala.swing.event.ButtonClicked

object MainGui extends SimpleSwingApplication {
  def top = new MainFrame {
    title = GuiSettings.MainScreenTitle
    preferredSize = GuiSettings.WindowSize

    val statsButton = new Button {
      text = "Get Pokemon Stats!"
      border = BorderFactory.createLineBorder(Color.black)
      background = Color.white
      borderPainted = true
      enabled = true
    }
    val encounterButton = new Button {
      text = "Generate an Encounter!"
      border = BorderFactory.createLineBorder(Color.black)
      background = Color.red
      borderPainted = true
      enabled = true
    }

    val gridPanel = new GridPanel(1, 2) {
      contents += statsButton
      contents += encounterButton
    }

    contents = new BorderPanel() {
      background = Color.black
      add(gridPanel, BorderPanel.Position.Center)
    }

    menuBar = GuiSettings.menuBar(GuiSettings.MainScreenTitle, this)

    listenTo(statsButton, encounterButton)

    reactions += {
      case ButtonClicked(component) if component == statsButton =>
        PokemonStatsGui.startup(Array.empty[String])
        this.dispose()
      case ButtonClicked(component) if component == encounterButton =>
        EncounterGui.startup(Array.empty[String])
        this.dispose()
    }
  }
}
