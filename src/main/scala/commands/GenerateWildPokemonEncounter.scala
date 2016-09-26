package commands

import models.Random

object GenerateWildPokemonEncounter extends App {
  // args: Option[level], Option[choices], Option[biome]
  val maybeLevel = args.headOption
  val numberOfChoices =
    (if (args.nonEmpty) args.tail.headOption.map(_.toInt)
    else None).getOrElse(1)
  val maybeBiome = if (args.nonEmpty && args.tail.nonEmpty) args.lastOption else None

  // TODO: Sort the list of Pokemon into Biome lists, and allow for narrowing on that

  for (_ <- 1 to numberOfChoices) yield {
    val randomPokemon = Random.rollForRandomPokemon
    val fullStatsQuery = scala.io.StdIn.readLine(s"${randomPokemon.capitalize}? y/n\n")
    if (fullStatsQuery == "n") () // if no, pass, else full stats
    else commands.GetPokemonDnDStats.main(Array(randomPokemon, maybeLevel.getOrElse(1).toString))
  }
}
