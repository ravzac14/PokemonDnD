package data

object Types extends Enumeration {
  val Normal, Fighting, Flying, Poison, Ground, Rock, Bug, Ghost, Steel, Fire, Water, Grass, Electric, Psychic, Ice, Dragon, Dark, Fairy = Value

  def fromPsvRowItem(s: String): Seq[Types.Value] =
    s.trim.toLowerCase.replaceAll(" ","").split(",").map(fromString)

  def fromString(s: String): Types.Value = s.trim.toLowerCase match {
    case "normal" => Normal
    case "fighting" => Fighting
    case "flying" => Flying
    case "poison" => Poison
    case "ground" => Ground
    case "rock" => Rock
    case "bug" => Bug
    case "ghost" => Ghost
    case "steel" => Steel
    case "fire" => Fire
    case "water" => Water
    case "grass" => Grass
    case "electric" => Electric
    case "psychic" => Psychic
    case "ice" => Ice
    case "dragon" => Dragon
    case "dark" => Dark
    case "fairy" => Fairy
    case e => throw new Exception(s"Unknown Type [$e]!")
  }

  def chaModFor(t: Types.Value): Int = t match {
    case Ground => -2
    case Rock => -2
    case Steel => -2
    case Poison => -1
    case Psychic => -1
    case Fighting => -1
    case Water => -1
    case Ice => 0
    case Normal => 0
    case Bug => 0
    case Grass => 0
    case Electric => 1
    case Fire => 1
    case Flying => 1
    case Dark => 1
    case Ghost => 2
    case Dragon => 2
    case Fairy => 2
    case e => throw new Exception(s"Unknown Type [$e]!")
  }

  def weakTo: PartialFunction[Types.Value, Seq[Types.Value]] = {
    case Types.Normal => Seq(Types.Fighting)
    case Types.Fighting => Seq(Types.Flying, Types.Psychic, Types.Fairy)
    case Types.Flying => Seq(Types.Rock, Types.Electric, Types.Ice)
    case Types.Poison => Seq(Types.Ground, Types.Psychic)
    case Types.Ground => Seq(Types.Water, Types.Grass, Types.Ice)
    case Types.Rock => Seq(Types.Fighting, Types.Ground, Types.Steel, Types.Water, Types.Grass)
    case Types.Bug => Seq(Types.Flying, Types.Rock, Types.Fire)
    case Types.Ghost => Seq(Types.Ghost, Types.Dark)
    case Types.Steel => Seq(Types.Fighting, Types.Ground, Types.Fire)
    case Types.Fire => Seq(Types.Ground, Types.Rock, Types.Water)
    case Types.Water => Seq(Types.Grass, Types.Electric)
    case Types.Grass => Seq(Types.Flying, Types.Poison, Types.Bug, Types.Fire, Types.Ice)
    case Types.Electric => Seq(Types.Ground)
    case Types.Psychic => Seq(Types.Bug, Types.Ghost, Types.Dark)
    case Types.Ice => Seq(Types.Fighting, Types.Rock, Types.Steel, Types.Fire)
    case Types.Dragon => Seq(Types.Ice, Types.Dragon, Types.Fairy)
    case Types.Dark => Seq(Types.Fighting, Types.Bug, Types.Fairy)
    case Types.Fairy => Seq(Types.Poison, Types.Steel)
  }

  def immuneTo: PartialFunction[Types.Value, Seq[Types.Value]] = {
    case Types.Normal => Seq(Types.Ghost)
    case Types.Fighting => Seq()
    case Types.Flying => Seq(Types.Ground)
    case Types.Poison => Seq()
    case Types.Ground => Seq(Types.Electric)
    case Types.Rock => Seq()
    case Types.Bug => Seq()
    case Types.Ghost => Seq(Types.Normal, Types.Fighting)
    case Types.Steel => Seq(Types.Poison)
    case Types.Fire => Seq()
    case Types.Water => Seq()
    case Types.Grass => Seq()
    case Types.Electric => Seq()
    case Types.Psychic => Seq()
    case Types.Ice => Seq()
    case Types.Dragon => Seq()
    case Types.Dark => Seq(Types.Psychic)
    case Types.Fairy => Seq(Types.Dragon)
  }

  def resists: PartialFunction[Types.Value, Seq[Types.Value]] = {
    case Types.Normal => Seq()
    case Types.Fighting => Seq(Types.Rock, Types.Bug)
    case Types.Flying => Seq(Types.Fighting, Types.Bug, Types.Grass)
    case Types.Poison => Seq(Types.Fighting, Types.Poison, Types.Bug, Types.Grass, Types.Fairy)
    case Types.Ground => Seq(Types.Poison, Types.Rock)
    case Types.Rock => Seq(Types.Normal, Types.Flying, Types.Poison, Types.Fire)
    case Types.Bug => Seq(Types.Fighting, Types.Ground, Types.Grass)
    case Types.Ghost => Seq(Types.Poison, Types.Bug)
    case Types.Steel => Seq(Types.Normal, Types.Flying, Types.Rock, Types.Bug, Types.Steel, Types.Grass, Types.Psychic, Types.Ice, Types.Dragon, Types.Fairy)
    case Types.Fire => Seq(Types.Bug, Types.Steel, Types.Fire, Types.Grass, Types.Ice, Types.Fairy)
    case Types.Water => Seq(Types.Steel, Types.Fire, Types.Water, Types.Ice)
    case Types.Grass => Seq(Types.Ground, Types.Water, Types.Electric, Types.Grass)
    case Types.Electric => Seq(Types.Flying, Types.Steel, Types.Electric)
    case Types.Psychic => Seq(Types.Fighting, Types.Psychic)
    case Types.Ice => Seq(Types.Ice)
    case Types.Dragon => Seq(Types.Fire, Types.Water, Types.Grass, Types.Electric)
    case Types.Dark => Seq(Types.Ghost, Types.Dark)
    case Types.Fairy => Seq(Types.Fighting, Types.Bug, Types.Dark)
  }
}