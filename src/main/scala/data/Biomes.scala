package data

object Biomes extends Enumeration {
  val Plains, Forest, Hills, Mountains, Marsh, Desert, Underground, Aquatic, Abyss, Urban = Value

  def fromString(s: String): Biomes.Value = s.trim.toLowerCase match {
    case "plains" | "plain" => Plains
    case "forest" | "forests" | "forrest" | "forrests" => Forest
    case "hills" | "hill" => Hills
    case "mountains" | "mountain" => Mountains
    case "marsh" | "marshland" | "marshes"  | "swamp" | "swampland" => Marsh
    case "desert" | "dessert" => Desert
    case "underground" | "under ground" | "underdark" | "under-dark" | "under-ground" => Underground
    case "aquatic" | "underwater" | "water" | "surfing" | "surf" => Aquatic
    case "abyss" => Abyss
    case "urban" | "city" | "town" => Urban // Split these up as you need
    case _ => throw new IllegalArgumentException(s"Unknown biome given, $s... The Biomes are: \n${Biomes.values}")
  }

  lazy val typesForBiome: Map[Biomes.Value, Seq[String]] = Map(
    Plains -> Seq("Normal", "Fighting", "Flying", "Grass", "Bug", "Electric", "Ground", "Ghost", "Psychic", "Fairy"),
    Forest -> Seq("Normal", "Flying", "Poison", "Bug", "Ghost", "Grass", "Fairy"),
    Hills -> Seq("Normal", "Fighting", "Flying", "Grass", "Steel", "Rock", "Ground", "Electric", "Fairy"),
    Mountains -> Seq("Normal", "Fighting", "Flying", "Ground", "Rock", "Steel", "Fire", "Ice", "Dragon"),
    Marsh -> Seq("Normal", "Poison", "Bug", "Ghost", "Water", "Grass", "Psychic", "Dragon"),
    Desert -> Seq("Normal", "Fighting", "Flying", "Ground", "Rock", "Steel", "Fire"),
    Underground -> Seq("Poison", "Ground", "Rock", "Ghost", "Steel", "Fire", "Water", "Psychic", "Ice", "Dragon", "Dark", "Fairy"),
    Aquatic -> Seq("Water", "Ice"),
    Abyss -> Seq("Poison", "Ghost", "Steel", "Psychic", "Ice", "Dragon", "Dark", "Fairy"),
    Urban -> Seq("Normal", "Fighting", "Flying", "Poison", "Ghost", "Steel", "Electric", "Psychic", "Dark", "Fairy"))

  lazy val pokemonByBiome: Map[Biomes.Value, Seq[String]] = typesForBiome.map {
    case (biome, types) => biome -> types.flatMap(PokemonList.pokemonByType.get).flatten
  }
}