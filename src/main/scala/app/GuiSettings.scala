package app

import java.awt.Color
import javax.swing.BorderFactory
import scala.swing._

object GuiSettings {
  val MainScreenTitle = "Pokemon D&D DM Tools!"
  val PokemonStatsScreenTitle = "Get Pokemon D&D Stats!"
  val EncounterScreenTitle = "Generate a Pokemon D&D Encounter!"
  val WindowSize = new Dimension(400, 200)

  def enterTextField(defaultText: String) = new TextArea {
    text = defaultText
    border = BorderFactory.createLineBorder(Color.black)
    background = Color.white
  }

  def dropDownField[S](defaultText: String, items: Seq[S]) = new ComboBox[S](items) {
    tooltip = defaultText
    border = BorderFactory.createLineBorder(Color.black)
    background = Color.white
  }

  def copyableOutput = new TextArea {
    text = "Output"
    border = BorderFactory.createLineBorder(Color.black)
    background = Color.red
  }

  def submitButton = new Button {
    text = "Submit"
    foreground = Color.white
    border = BorderFactory.createLineBorder(Color.red)
    background = Color.black
    borderPainted = true
    enabled = true
  }

  def menuBar(screenTitle: String, frame: MainFrame) = screenTitle match {
    case MainScreenTitle => new MenuBar {
      contents += new Menu("Actions") {
        contents += new MenuItem(Action("Get Pokemon Stats") {
          PokemonStatsGui.startup(Array.empty[String])
          frame.dispose()
        })
        contents += new MenuItem(Action("Generate Encounter") {
          EncounterGui.startup(Array.empty[String])
          frame.dispose()
        })
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
    case PokemonStatsScreenTitle => new MenuBar {
      contents += new Menu("Actions") {
        contents += new MenuItem(Action("Main Screen") {
          MainGui.startup(Array.empty[String])
          frame.dispose()
        })
        contents += new MenuItem(Action("Generate Encounter") {
          EncounterGui.startup(Array.empty[String])
          frame.dispose()
        })
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
    case EncounterScreenTitle => new MenuBar {
      contents += new Menu("Actions") {
        contents += new MenuItem(Action("Main Screen") {
          MainGui.startup(Array.empty[String])
          frame.dispose()
        })
        contents += new MenuItem(Action("Get Pokemon D&D Stats") {
          PokemonStatsGui.startup(Array.empty[String])
          frame.dispose()
        })
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
    case _ => new MenuBar {
      contents += new Menu("Actions") {
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
  }
}