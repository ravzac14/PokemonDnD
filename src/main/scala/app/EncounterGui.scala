package app

import java.io.{PrintWriter, File}

import commands.GetPokemonDnDStats
import data.Biomes
import models.Random
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTimeZone, DateTime}

import scala.swing.event.ButtonClicked
import scala.swing._

object EncounterGui extends SimpleSwingApplication {

  def genEncounterToFile(level: Int, numTargets: Int, biome: Biomes.Value): File = {
    def toLocalFile(pokemonStatStrings: Seq[String]): File = {
      val now = new DateTime(DateTimeZone.forID("America/Los_Angeles"))
      val nowAsString = DateTimeFormat.forPattern("YYYY-MM-dd").print(now)
      val file = new File(s"${biome}_level-${level}_targets-${numTargets}_$nowAsString.txt")
      new PrintWriter(file) {
        write(pokemonStatStrings.mkString("\n"))
        close()
      }

      file
    }

    val pokemonStrings =
      for (_ <- 1 to numTargets) yield {
        val randomPokemon = Random.rollForRandomPokemonFromBiome(biome)
        GetPokemonDnDStats.statsAsString(randomPokemon, level.toInt, autoLevelUp = true, verbose = false)
      }

    toLocalFile(pokemonStrings)
  }

  def top = new MainFrame {
    title = GuiSettings.EncounterScreenTitle
    preferredSize = GuiSettings.WindowSize
    val levelDropDown = GuiSettings.dropDownField(
      defaultText = "Desired level...",
      items = 1 to 20)
    val numMonstersDropDown = GuiSettings.dropDownField(
      defaultText = "Desired size of encounter...",
      items = 1 to 50)
    val biomeDropDown = GuiSettings.dropDownField(
      defaultText = "Desired biome...",
      items = Biomes.values.toSeq.sorted)
    val output = GuiSettings.copyableOutput
    val button = GuiSettings.submitButton

    val gridPanel = new GridPanel(1, 3) {
      contents += levelDropDown
      contents += numMonstersDropDown
      contents += biomeDropDown
    }

    contents = new BorderPanel() {
      add(gridPanel, BorderPanel.Position.North)
      add(output, BorderPanel.Position.Center)
      add(button, BorderPanel.Position.South)
    }

    menuBar = GuiSettings.menuBar(GuiSettings.EncounterScreenTitle, this)

    listenTo(button)

    reactions += {
      case ButtonClicked(component) if component == button =>
        val file = genEncounterToFile(
          level = levelDropDown.selection.item,
          numTargets = numMonstersDropDown.selection.item,
          biome = biomeDropDown.selection.item)
        output.text_=(file.getAbsolutePath)
    }
  }
}