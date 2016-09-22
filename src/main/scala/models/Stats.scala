package models

object HitDice extends Enumeration { val d6, d8, d10, d12, d20 = Value }

case class PokemonBaseStats(
  val hp: Int,
  val attack: Int,
  val defense: Int,
  val spAttack: Int,
  val spDefense: Int,
  val speed: Int) {
  def prettyPrint =
    s"""%%%%%%%% Pokemon Base Stats %%%%%%%%
       |HP: $hp
       |Attack: $attack
       |Defense: $defense
       |Special Attack: $spAttack
       |Special Defense: $spDefense
       |Speed: $speed""".stripMargin
}

case class DnDStats(
  val level: Int = 1,
  val hitDice: HitDice.Value,
  val rolledMaxHp: Int,
  val maxPossibleHp: Int,
  val armorClass: Int,
  val strength: Int,
  val dexterity: Int,
  val constitution: Int,
  val intelligence: Int,
  val wisdom: Int,
  val charisma: Int,
  val movementSpeed: Int) {
  def prettyPrint =
    s"""^^^^^^^^^ Pokemon DnD Stats ^^^^^^^^^
       |Level: $level
       |Hit Dice (for this evolution): $hitDice
       |Rolled Max Health: $rolledMaxHp
       |Max Possible HP: $maxPossibleHp
       |AC: $armorClass
       |Movement Speed (for this evolution): $movementSpeed
       |Strength: $strength
       |Dexterity: $dexterity
       |Constitution: $constitution
       |Intelligence: $intelligence
       |Wisdom: $wisdom
       |Charisma: $charisma""".stripMargin
}

object StatTransformers {
  def roundUpToNearestTenAndDrop(i: Int): Int = Math.round((i + 5) / 10)

  def nearestHitDice(roundedHp: Int): HitDice.Value = roundedHp match {
    case i if i >= 18 => HitDice.d20
    case i if i >= 11 => HitDice.d12
    case i if i >= 8 => HitDice.d10
    case i if i >= 6 => HitDice.d8
    case _ => HitDice.d6
  }

  // A small positive or negative number to move other stats
  def statToModifier(stat: Int): Int = stat match {
    case v if v >= 30 => 10
    case v if v >= 28 => 9
    case v if v >= 26 => 8
    case v if v >= 24 => 7
    case v if v >= 22 => 6
    case v if v >= 20 => 5
    case v if v >= 18 => 4
    case v if v >= 16 => 3
    case v if v >= 14 => 2
    case v if v >= 12 => 1
    case v if v >= 10 => 0
    case v if v >= 8 => -1
    case v if v >= 6 => -2
    case v if v >= 4 => -3
    case v if v >= 2 => -4
    case _ => -5
  }

  def movementSpeed(dex: Int): Int = (Math.ceil((dex / 2.5) / 5) * 5).toInt * 3

  def maxHp(hitDice: HitDice.Value, level: Int, constitution: Int, maxPossible: Boolean) = {
    val conMod = statToModifier(constitution)
    val base = hitDice match {
      case HitDice.d20 => 20
      case HitDice.d12 => 12
      case HitDice.d10 => 10
      case HitDice.d8 => 8
      case HitDice.d6 => 6
    }
    if (level > 1) {
      val rest = for (_ <- 1 to level) yield {
        val roll =
          if (maxPossible) base
          else hitDice match {
            case HitDice.d20 => Dice.D20RollOrAverage
            case HitDice.d12 => Dice.D12RollOrAverage
            case HitDice.d10 => Dice.D10RollOrAverage
            case HitDice.d8 => Dice.D8RollOrAverage
            case HitDice.d6 => Dice.D6RollOrAverage
          }
        roll + conMod
      }
      (base + conMod) + rest.sum
    } else base + conMod
  }

  def pokemonToDndStats(p: PokemonBaseStats, level: Int = 1): DnDStats = {
    val hitDice = nearestHitDice(roundUpToNearestTenAndDrop(p.hp))
    val str = roundUpToNearestTenAndDrop(p.attack) + 3
    val dex = roundUpToNearestTenAndDrop(p.speed) + 3
    val con  = roundUpToNearestTenAndDrop((p.defense + p.hp) / 2)
    val int = roundUpToNearestTenAndDrop(p.spAttack) + 3
    val wis = roundUpToNearestTenAndDrop(Math.max(p.spAttack, p.spDefense)) + 3

    DnDStats(
      level = level,
      hitDice = hitDice,
      rolledMaxHp = maxHp(hitDice, level, con, maxPossible = false),
      maxPossibleHp = maxHp(hitDice, level, con, maxPossible = true),
      armorClass = Math.max(10, Math.max(dex, Math.max(con, roundUpToNearestTenAndDrop(p.defense) + 3))),
      strength = str,
      dexterity = dex,
      constitution = con,
      intelligence = int,
      wisdom = wis,
      charisma = (str + dex + con + int + wis) / 5,
      movementSpeed = movementSpeed(dex)
    )
  }
}

