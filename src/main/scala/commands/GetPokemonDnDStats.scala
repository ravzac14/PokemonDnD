package commands

import java.io.{PrintWriter, File}

import data.PokemonList
import models.{PokemonMove, PokemonBaseStats, StatTransformers}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model.{Element, Document}
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat

import scala.util.{Failure, Success, Try}

object GetPokemonDnDStats extends App {
  def getPokemonStatsFromDoc(doc: Document): PokemonBaseStats = {
    val baseStats = (doc >> elementList(".vitals-table")).drop(3).head
    val statList = baseStats >> elementList("tr")
    models.PokemonBaseStats(
      hp = (statList.drop(1).head >> text("td")).toInt,
      attack = (statList.drop(2).head >> text("td")).toInt,
      defense = (statList.drop(3).head >> text("td")).toInt,
      spAttack = (statList.drop(4).head >> text("td")).toInt,
      spDefense = (statList.drop(5).head >> text("td")).toInt,
      speed = (statList.drop(6).head >> text("td")).toInt)
  }

  // Doesn't work for eevee-lutions or evolution lines with baby Pokemon
  def getPokemonCRFromDoc(doc: Document, pokemon: String): (Int, Int) = { // (CR, Level for next Evolution)
    val maybeEvolution: Option[Element] = (doc >> elementList(".infocard-evo-list")).headOption
    val maybeEvos: Option[Seq[Element]] = maybeEvolution.map(e =>
      (e >> elementList(".infocard-tall")).flatMap(_ >> elementList(".sprite")))
    val maybeEvoBlock: Option[String] = maybeEvolution.map(e =>
      (e >> elementList(".infocard-tall")).mkString(""))
    val levelRegex = "(\\(Level [0-9]+\\))".r
    val maybeLevelList: Option[Seq[Int]] = maybeEvoBlock
      .map(eb => for (m <- levelRegex findAllMatchIn eb) yield m group 1)
      .map(_.toSeq.map(_.replaceAll("\\(Level ","").replaceAll("\\)","").toInt))

    val maybeEvoNumber: Option[Int] = maybeEvos.map { e =>
      e.map(_.toString.replaceAll(".*alt=\"","").toLowerCase).takeWhile(!_.startsWith(pokemon)).length + 1
    }
    val evoMultiple: Float = maybeEvoNumber match {
      case Some(1) if maybeEvos.get.length > 1 => .5f
      case Some(2) if maybeEvos.get.length > 2 => 1f
      case _ => 2f
    }
    val maybeEvoLevel: Option[Int] = for {
      evoNumber <- maybeEvoNumber
      levelList <- maybeLevelList
      if evoNumber <= levelList.length
    } yield levelList(evoNumber - 1)

    val dataStats: Element = (doc >> elementList(".vitals-table")).head
    val dataList: Seq[String] = (dataStats >> elementList("tr")).drop(1) >> text("td")
    val types: Seq[String] = PokemonList.typesForPokemon.get(pokemon.capitalize).get
    val typeMultiple: Int = types.length
    val height: Float = dataList.drop(2).head.replaceAll(" ", "").replaceAll(".*\\(","").replaceAll("m\\)","").toFloat
    val weight: Float = dataList.drop(3).head.replaceAll(" ", "").replaceAll(".*\\(","").replaceAll("kg\\)","").toFloat

    val eggStats: Element = ((doc >> elementList(".vitals-table")).drop(2).head >> elementList("tr")).head
    val eggGroups: Seq[String] =
      (eggStats >> elementList("td")).map(e => (e >> text("td")).toLowerCase).head.replaceAll(",", "").split(" ")
    val updatedEggGroups: Seq[String] = eggGroups.foldLeft(Seq.empty[String]) {case (acc, v) => v.trim match {
      case i if i == "1" || i == "2" || i == "3" => acc.init :+ (acc.last.trim + v.trim)
      case _ => acc :+ v
    }}

    val B = Math.ceil(height.toDouble)
    val C = B + StatTransformers.getEggGroupBonus(updatedEggGroups).toDouble
    val D = C * typeMultiple
    val E = D * evoMultiple
    (Math.ceil(E + StatTransformers.getWeightBonus(weight)).toInt, maybeEvoLevel.getOrElse(0))
  }

  def getPokemonMovesFromDoc(doc: Document): Seq[PokemonMove] = {
    val movesByLevelTable: Seq[Element] = ((doc >> elementList(".wide-table")).head >> elementList("tr")).tail
    movesByLevelTable.map { e =>
      val l: Int = (e >> text(".num")).trim.toInt
      val move: String = (e >> text(".ent-name")).trim
      PokemonMove(l, move.toLowerCase)
    }
  }

  def statsAsString(pokemon: String, level: Int): String = {
    val tryStatsString =
      for {
        doc <- Try {
          val browser = JsoupBrowser()
          browser.get(s"http://pokemondb.net/pokedex/$pokemon")
        }
        pokemonStats <- Try(getPokemonStatsFromDoc(doc))
        (pokemonCR, pokemonEvoLevel) <- Try(getPokemonCRFromDoc(doc, pokemon))
        pokemonMoves <- Try(getPokemonMovesFromDoc(doc))
        dndStats <- Try(StatTransformers.pokemonToDndStats(
          name = pokemon,
          p = pokemonStats,
          cr = pokemonCR,
          evoLevel = pokemonEvoLevel,
          moves = pokemonMoves,
          level = level))
      } yield
        s"""|${pokemonStats.prettyPrint}
            |
            |${dndStats.prettyPrint}
            |
            |""".stripMargin

    val result = tryStatsString match {
      case Success(s) => s
      case Failure(e) => e.getMessage
    }

    s"""%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       |%%%%                  ${pokemon.toUpperCase}
       |%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       |
       |$result
       |
     """.stripMargin
  }

  def statsAsFile(pokemon: String, level: Int, path: Option[String] = None): File = {
    val statString = statsAsString(pokemon, level)
    val now = new DateTime(DateTimeZone.forID("America/Los_Angeles"))
    val nowAsString = DateTimeFormat.forPattern("YYYY-MM-dd").print(now)
    val file = new File(path.getOrElse(s"${pokemon}_${level}_$nowAsString.txt"))
    new PrintWriter(file) {
      write(statString)
      close()
    }
    file
  }

  override def main(args: Array[String]) {
    val pokemon = args.head.trim.toLowerCase
    require(data.PokemonList.pokemonKeyList.contains(pokemon), "That Pokemon doesn't exist!")
    val level = args.drop(1).headOption.map(_.toInt).getOrElse(1)
    val statsString = statsAsString(pokemon, level)

    println(statsString)
  }
}