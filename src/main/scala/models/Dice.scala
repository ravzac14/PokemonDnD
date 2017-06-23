package models

import data.Biomes

object Dice {
  val D20Average = 11
  val D12Average = 7
  val D10Average = 6
  val D8Average = 5
  val D6Average = 4
  val D4Average = 3

  private val r = scala.util.Random

  def D20Roll = r.nextInt(20) + 1
  def D12Roll = r.nextInt(12) + 1
  def D10Roll = r.nextInt(10) + 1
  def D8Roll = r.nextInt(8) + 1
  def D6Roll = r.nextInt(6) + 1
  def D4Roll = r.nextInt(4) + 1

  def D20RollOrAverage = Math.max(D20Roll, D20Average)
  def D12RollOrAverage = Math.max(D12Roll, D12Average)
  def D10RollOrAverage = Math.max(D10Roll, D10Average)
  def D8RollOrAverage = Math.max(D8Roll, D8Average)
  def D6RollOrAverage = Math.max(D6Roll, D6Average)
  def D4RollOrAverage = Math.max(D4Roll, D4Average)
}

object Random {
  def rollForRandomMove(availableMoves: Seq[DnDMove]): DnDMove = {
    val r = scala.util.Random
    availableMoves(r.nextInt(availableMoves.length - 1))
  }

  def rollForRandomPokemon: String = {
    val r = scala.util.Random
    val randomNumber = r.nextInt(data.PokemonList.totalPokemon) + 1
    data.PokemonList.indexMapWithTypes
      .find { case (k, (n, ts)) => k.dropWhile(_ == '0').toInt == randomNumber }
      .map(_._2._1)
      .getOrElse("Togepi") //lol
  }

  def rollForRandomPokemonFromBiome(biome: Biomes.Value): String = {
    val r = scala.util.Random
    val pokemonFromBiome = data.Biomes.pokemonByBiome.getOrElse(biome, Seq())
    require(pokemonFromBiome.nonEmpty, s"Given biome, $biome was not found in biome list: \n${Biomes.values}")

    val randomNumber = r.nextInt(pokemonFromBiome.length)
    pokemonFromBiome(randomNumber)
  }
}