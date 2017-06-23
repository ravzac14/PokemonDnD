package app

import java.io.{PrintWriter, File}

import commands.GetPokemonDnDStats
import data.PokemonList
import org.joda.time.{DateTime, DateTimeZone}

import scala.swing._
import scala.swing.event.ButtonClicked
import scala.util.{Failure, Success, Try}

object PokemonStatsGui extends SimpleSwingApplication {
  def statsAsFile(
    pokemon: String,
    level: Int,
    path: Option[String] = None,
    autoLevelUp: Boolean = false,
    shinyMove: Boolean = false): String = {
    val statString =
      Try {
        GetPokemonDnDStats.statsAsString(
          pokemon,
          level,
          autoLevelUp = autoLevelUp,
          verbose = true,
          shinyMove = shinyMove)
      }
    val now = new DateTime(DateTimeZone.forID("America/Los_Angeles"))
    val nowAsString = now.toString("yyyy-MM-dd")
    val file = new File(path.getOrElse(s"${pokemon.capitalize}_${level}_$nowAsString.txt"))
    val result = statString.flatMap { s =>
      Try {
        new PrintWriter(file) {
          write(s)
          close()
        }
      }
    }
    result match {
      case Success(_) => file.getAbsolutePath
      case Failure(ex) => ex.getMessage
    }
  }

  def top = new MainFrame {
    title = GuiSettings.PokemonStatsScreenTitle
    preferredSize = GuiSettings.WindowSize
    val nameDropDown = GuiSettings.dropDownField(
      defaultText = "Pokemon name...",
      items = PokemonList.pokemonList.sorted)
    val levelDropDown = GuiSettings.dropDownField(
      defaultText = "Desired level...",
      items = 1 to 20)
    val output = GuiSettings.copyableOutput
    val button = GuiSettings.submitButton
    val autoLevelCheckbox = GuiSettings.checkbox("Auto Level-Up?")
    val shinyCheckbox = GuiSettings.checkbox("Find A Shiny Move?")

    val gridPanel = new GridPanel(2, 2) {
      contents += nameDropDown
      contents += levelDropDown
      contents += autoLevelCheckbox
      contents += shinyCheckbox
    }

    contents = new BorderPanel() {
      add(gridPanel, BorderPanel.Position.North)
      add(output, BorderPanel.Position.Center)
      add(button, BorderPanel.Position.South)
    }

    menuBar = GuiSettings.menuBar(GuiSettings.PokemonStatsScreenTitle, this)

    listenTo(button)

    reactions += {
      case ButtonClicked(component) if component == button =>
        val filePath = statsAsFile(
          pokemon = PokemonList.nameKey(nameDropDown.selection.item),
          level = levelDropDown.selection.item,
          autoLevelUp = autoLevelCheckbox.enabled,
          shinyMove = shinyCheckbox.enabled)
        output.text_=(filePath)
    }
  }
}
