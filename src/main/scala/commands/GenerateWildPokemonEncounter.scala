package commands

import data.Biomes
import models.Random

object GenerateWildPokemonEncounter extends App {
  val level = args.head
  val numberOfChoices = args.tail.head.toInt
  val biome = Biomes.fromString(args.last.trim)

  def genInteractive =
    for (_ <- 1 to numberOfChoices) yield {
      val randomPokemon = Random.rollForRandomPokemonFromBiome(biome)
      val fullStatsQuery = scala.io.StdIn.readLine(s"Found ${randomPokemon.capitalize}. Do you want stats? y/n/v\n")
      if (fullStatsQuery == "n") () // if no, pass, else full stats
      else if (fullStatsQuery == "v") {
        val stats = GetPokemonDnDStats.statsAsString(randomPokemon, level.toInt, autoLevelUp = true, verbose = true)
        println(stats)
      }
      else {
        val stats = GetPokemonDnDStats.statsAsString(randomPokemon, level.toInt, autoLevelUp = true, verbose = false)
        println(stats)
      }
    }

  genInteractive
}
