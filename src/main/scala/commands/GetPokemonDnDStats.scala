package commands

import models.{Dice, StatTransformers}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

object GetPokemonDnDStats extends App {
  val pokemon = args.head.trim.toLowerCase

  val browser = JsoupBrowser()
  val doc = browser.get(s"http://pokemondb.net/pokedex/$pokemon")

  val baseStats = (doc >> elementList(".vitals-table")).drop(3).head
  val statList = baseStats >> elementList("tr")
  val pokemonStats = models.PokemonBaseStats(
    hp = (statList.drop(1).head >> text("td")).toInt,
    attack = (statList.drop(2).head >> text("td")).toInt,
    defense = (statList.drop(3).head >> text("td")).toInt,
    spAttack = (statList.drop(4).head >> text("td")).toInt,
    spDefense = (statList.drop(5).head >> text("td")).toInt,
    speed = (statList.drop(6).head >> text("td")).toInt)

  val dndStats = StatTransformers.pokemonToDndStats(pokemonStats)

  println()
  println()
  println(pokemonStats.prettyPrint)
  println()
  println()
  println(dndStats.prettyPrint)
}