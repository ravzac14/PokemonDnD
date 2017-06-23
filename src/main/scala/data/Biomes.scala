package data

object Biomes extends Enumeration {
  val ANY, Plains, Forest, Hills, Mountains, Marsh, Desert, Underground, Aquatic, Abyss, Urban = Value

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
    case "none"  | "any" => ANY
    case _ => throw new IllegalArgumentException(s"Unknown biome given, $s... The Biomes are: \n${Biomes.values}")
  }

  lazy val typesForBiome: Map[Biomes.Value, Seq[Types.Value]] = Map(
    Plains -> Seq(Types.Normal, Types.Fighting, Types.Flying, Types.Grass, Types.Bug, Types.Electric, Types.Ground, Types.Ghost, Types.Psychic, Types.Fairy),
    Forest -> Seq(Types.Normal, Types.Flying, Types.Poison, Types.Bug, Types.Ghost, Types.Grass, Types.Fairy),
    Hills -> Seq(Types.Normal, Types.Fighting, Types.Flying, Types.Grass, Types.Steel, Types.Rock, Types.Ground, Types.Electric, Types.Fairy),
    Mountains -> Seq(Types.Normal, Types.Fighting, Types.Flying, Types.Ground, Types.Rock, Types.Steel, Types.Fire, Types.Ice, Types.Dragon),
    Marsh -> Seq(Types.Normal, Types.Poison, Types.Bug, Types.Ghost, Types.Water, Types.Grass, Types.Psychic, Types.Dragon),
    Desert -> Seq(Types.Normal, Types.Fighting, Types.Flying, Types.Ground, Types.Rock, Types.Steel, Types.Fire),
    Underground -> Seq(Types.Poison, Types.Ground, Types.Rock, Types.Ghost, Types.Steel, Types.Fire, Types.Water, Types.Psychic, Types.Ice, Types.Dragon, Types.Dark, Types.Fairy),
    Aquatic -> Seq(Types.Water, Types.Ice),
    Abyss -> Seq(Types.Poison, Types.Ghost, Types.Steel, Types.Psychic, Types.Ice, Types.Dragon, Types.Dark, Types.Fairy),
    Urban -> Seq(Types.Normal, Types.Fighting, Types.Flying, Types.Poison, Types.Ghost, Types.Steel, Types.Electric, Types.Psychic, Types.Dark, Types.Fairy),
    ANY -> Types.values.toSeq)

  lazy val pokemonByBiome: Map[Biomes.Value, Seq[String]] = typesForBiome.map {
    case (biome, types) => biome -> types.flatMap(PokemonList.pokemonByType.get).flatten
  }
}