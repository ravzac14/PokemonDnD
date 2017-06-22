package app

import java.io.{PrintWriter, File}

import commands.GetPokemonDnDStats
import data.PokemonList
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}

import scala.swing._
import scala.swing.event.ButtonClicked

object PokemonStatsGui extends SimpleSwingApplication {
  def statsAsFile(pokemon: String, level: Int, path: Option[String] = None): File = {
    val statString = GetPokemonDnDStats.statsAsString(pokemon, level, autoLevelUp = true, verbose = true)
    val now = new DateTime(DateTimeZone.forID("America/Los_Angeles"))
    val nowAsString = DateTimeFormat.forPattern("YYYY-MM-dd").print(now)
    val file = new File(path.getOrElse(s"${pokemon}_${level}_$nowAsString.txt"))
    new PrintWriter(file) {
      write(statString)
      close()
    }
    file
  }

  def top = new MainFrame {
    title = GuiSettings.PokemonStatsScreenTitle
    preferredSize = GuiSettings.WindowSize
    val nameDropDown = GuiSettings.dropDownField(
      defaultText = "Pokemon name...",
      items = PokemonList.pokemonKeyList.map(_.capitalize).sorted)
    val levelDropDown = GuiSettings.dropDownField(
      defaultText = "Desired level...",
      items = 1 to 20)
    val output = GuiSettings.copyableOutput
    val button = GuiSettings.submitButton

    val gridPanel = new GridPanel(1, 2) {
      contents += nameDropDown
      contents += levelDropDown
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
        val file = statsAsFile(
          pokemon = nameDropDown.selection.item,
          level = levelDropDown.selection.item)
        output.text_=(file.getAbsolutePath)
    }
  }
}
