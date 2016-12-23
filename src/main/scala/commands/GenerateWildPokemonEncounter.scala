package commands

import data.Biomes
import models.Random

// TODO(zack): Make these use real parameter logic
object GenerateWildPokemonEncounter extends App {
  // args: Option[level], Option[choices], Option[biome]
  val maybeLevel = args.headOption
  require(
    requirement = !maybeLevel.map(_.toLowerCase).contains("help"),
    message =
      s"""No args are *required* but possible to run like:
         |
         |    GenerateWildPokemonEncounter 5 20 forest
         |
         |Which will generate 20 level 5 Pokemon from the forest biome.
         |
         |A no arg run defaults to generating 1 level 1 Pokemon of any biome.""".stripMargin)

  val numberOfChoices =
    (if (args.nonEmpty) args.tail.headOption.map(_.toInt)
    else None).getOrElse(1)
  val maybeBiome = {
    if (args.nonEmpty && args.tail.nonEmpty) args.lastOption else None
  }.map(Biomes.fromString)

  for (_ <- 1 to numberOfChoices) yield {
    val randomPokemon =
      if (maybeBiome.isDefined) Random.rollForRandomPokemonFromBiome(maybeBiome.get)
      else Random.rollForRandomPokemon
    val fullStatsQuery = scala.io.StdIn.readLine(s"Found ${randomPokemon.capitalize}. Do you want stats? y/n\n")
    if (fullStatsQuery == "n") () // if no, pass, else full stats
    else commands.GetPokemonDnDStats.main(Array(randomPokemon, maybeLevel.getOrElse(1).toString))
  }
}
