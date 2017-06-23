package models

import data.{PokemonList, Types, MoveList}

object HitDice extends Enumeration { val d6, d8, d10, d12 = Value }

object DnDAbility extends Enumeration {
  val Strength, Dexterity, Constitution, Intelligence, Wisdom, Charisma = Value

  def fromPsvRowItem(s: String): Seq[DnDAbility.Value] =
    s.trim.toLowerCase.replaceAll(" ", "").split(",").map(fromString)

  def fromString(s: String): DnDAbility.Value = s.trim.toLowerCase.take(3) match {
    case "str" => Strength
    case "dex" => Dexterity
    case "con" => Constitution
    case "int" => Intelligence
    case "wis" => Wisdom
    case "cha" => Charisma
    case e => throw new Exception(s"Unknown DnDAbility [$e]!")
  }
}

case class PokemonMove(
  level: Int,
  name: String) {
  def nameKey = StatTransformers.stringKey(name)
  def simplePrint = s"level: $level -- ${nameKey.capitalize}"
  def isDefault = nameKey == "tackle" || nameKey == "scratch" || nameKey == "pound"
}

case class PokemonBaseStats(
  val hp: Int,
  val attack: Int,
  val defense: Int,
  val spAttack: Int,
  val spDefense: Int,
  val speed: Int,
  val moves: Seq[PokemonMove] = Seq.empty[PokemonMove]) {
  def prettyPrint =
    s"""^^^^^^^^ Pokemon Base Stats ^^^^^^^^^^
       |HP: $hp
       |Attack: $attack
       |Defense: $defense
       |Special Attack: $spAttack
       |Special Defense: $spDefense
       |Speed: $speed
       |Moves: ${moves.map("\n    " + _.simplePrint).mkString("")}""".stripMargin
}

case class DnDStats(
  val name: String,
  val level: Int = 1,
  val challengeRating: Int = 0,
  val nextEvolutionLevel: Option[Int] = None,
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
  val movementSpeed: Int,
  val availableMoves: Seq[DnDMove],
  val rolledMoves: Seq[DnDMove]) {
  def print =
    s"""Name: ${name.capitalize}
        |Level: $level
        |Challenge Rating (discount this for evolution lines with baby Pokemon or multiple forms): $challengeRating
        |Level of next evolution: ${nextEvolutionLevel match { case None => "N/A" case Some(e) => e.toString }}
        |Hit Dice (for this evolution): $hitDice
        |Max Health: $rolledMaxHp
        |AC: $armorClass
        |Movement Speed: $movementSpeed
        |Strength: $strength
        |Dexterity: $dexterity
        |Constitution: $constitution
        |Intelligence: $intelligence
        |Wisdom: $wisdom
        |Charisma: $charisma
        |Rolled Moves (for this evolution and level): ${rolledMoves.map("\n    " + _.simplePrint).mkString("")}""".stripMargin
  def prettyPrint =
    s"""^^^^^^^^^ Pokemon DnD Stats ^^^^^^^^^
        |Name: ${name.capitalize}
        |Level: $level
        |Challenge Rating (discount this for evolution lines with baby Pokemon or multiple forms): $challengeRating
        |Level of next evolution: ${nextEvolutionLevel match { case None => "N/A" case Some(e) => e.toString }}
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
        |Charisma: $charisma
        |All Pokemon Moves: ${availableMoves.map("\n    " + _.simplePrint).mkString("")}
        |Rolled Moves (for this evolution and level): ${rolledMoves.map("\n    " + _.simplePrint).mkString("")}""".stripMargin
}

object StatTransformers {
  def stringKey(s: String) = s.trim.toLowerCase

  private def roundUpToNearestTenAndDropAddThree(j: Int): Int = Math.round((j + 5) / 10) + 3

  private def trendTowardsMean(k: Int): Int = k match {
    case _ if k <= 6 => 6
    case _ if k >= 20 => 20
    case _ => k
  }

  def convertInitialStat(i: Int) = trendTowardsMean(roundUpToNearestTenAndDropAddThree(i))

  def getWeightBonus(w: Float): Int = w match {
    case i if i <= 5f => -2
    case i if i <= 50f => 0
    case i if i <= 60f => 2
    case i if i <= 70f => 5
    case i if i <= 80f => 10
    case i if i <= 90f => 15
    case i if i <= 100f => 20
    case i if i <= 150f => 30
    case i if i <= 200f => 40
    case i if i <= 250f => 50
    case i if i <= 300f => 60
    case _ => 100
  }

  def getEggGroupBonus(groups: Seq[String]): Int = {
    def groupToBonus(g: String) = g match {
      case "water1" => 1
      case "bug" => 1
      case "flying" => 3
      case "field" => 3
      case "monster" => 5
      case "fairy" => 5
      case "grass" => 5
      case "human-like" => 7
      case "water3" => 7
      case "mineral" => 7
      case "amorphous" => 9
      case "water2" => 9
      case "ditto" => 10
      case "dragon" => 20
      case "undiscovered" => 150
      case _ => throw new IllegalArgumentException(s"Unknown Egg Group: $g")
    }
    groups.map(groupToBonus).foldLeft(0)(Math.max)
  }

  def nearestHitDice(roundedHp: Int): HitDice.Value = roundedHp match {
    case i if i >= 12 => HitDice.d12
    case i if i >= 9 => HitDice.d10
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

  def rollForMoves(moves: Seq[DnDMove], level: Int): Seq[DnDMove] = {
    val levelCap = level * 5
    if (moves.length <= level + 1) (MoveList.default +: moves).distinct
    else {
      val availableMoves = moves.filterNot(_.isDefault)
      val base: Seq[DnDMove] = for (_ <- 0 to level) yield MoveList.default
      base.tail.foldLeft(Seq(base.head)) { case (acc, v) =>
        var rolledMove = Random.rollForRandomMove(availableMoves)
        while (rolledMove.isDefault || acc.map(_.name).contains(rolledMove.name)) {
          rolledMove = Random.rollForRandomMove(availableMoves)
        }
        acc :+ rolledMove
      }
    }
  }

  private def chaBase(statsMean: Int, types: Seq[Types.Value]): Int =
    statsMean + types.map(Types.chaModFor).sum

  def pokemonToDndMoves(moves: Seq[PokemonMove]): Seq[DnDMove] = {
    val asDnDMoves =
      moves.map {
        m =>
          MoveList.allMoves
            .find(_.nameKey == m.nameKey)
            .getOrElse(throw new Exception(s"Couldn't find a move ${m.nameKey}"))
            .copy(level = Some(Math.ceil(m.level / 5d).toInt))
      }
    val maybeFirstDefault = asDnDMoves.find(_.isDefault)
    maybeFirstDefault.toSeq ++ asDnDMoves.filterNot(_.isDefault)
  }

  def autoLevelUp(baseStats: DnDStats, level: Int): DnDStats = {
    def increment(stats: DnDStats, choice: Int) = choice match {
      case 1 => stats.copy(strength = stats.strength + 1)
      case 2 =>
        if (stats.dexterity + 1 >= stats.armorClass)
          stats.copy(dexterity = stats.dexterity + 1, armorClass = stats.dexterity + 1)
        else
          stats.copy(dexterity = stats.dexterity + 1)
      case 3 => stats.copy(constitution = stats.constitution + 1, rolledMaxHp = stats.rolledMaxHp + 1)
      case 4 => stats.copy(intelligence = stats.intelligence + 1)
      case 5 => stats.copy(wisdom = stats.wisdom + 1)
      case 6 => stats.copy(charisma = stats.charisma + 1)
      case 7 => stats.copy(movementSpeed = stats.movementSpeed + 5)
      case 8 if !stats.rolledMoves.forall(_.isDefault) =>
        val (defaults, other) = stats.rolledMoves.partition(_.isDefault)
        stats.copy(
          armorClass = stats.armorClass + 1,
          rolledMoves = defaults ++ other.init)
      case 8 => stats.copy(movementSpeed = stats.armorClass + 1)
    }

    if (level > 1)
      (1 to level).toSeq.foldLeft(baseStats) {
        case (stats, _) => increment(stats, scala.util.Random.nextInt(8) + 1)
      }
    else baseStats
  }

  def rollForRandomShinyMove(movesSoFar: Seq[DnDMove], types: Seq[Types.Value]): DnDMove = {
    def randomType = Types.values.toSeq(scala.util.Random.nextInt(Types.values.size))
    def randomMove(t: Types.Value) =
      MoveList.movesForType(t)(scala.util.Random.nextInt(MoveList.movesForType(t).length))
    var r = randomType
    while (types.contains(r)) { r = randomType }
    var s = randomMove(r)
    while (movesSoFar.contains(s)) { s = randomMove(r) }
    s
  }

  def pokemonToDndStats(
    name: String,
    p: PokemonBaseStats,
    cr: Int,
    maybeEvoLevel: Option[Int],
    level: Int = 1,
    autoUpLevel: Boolean = false,
    shinyMove: Boolean = false): DnDStats = {
    val hitDice = nearestHitDice(roundUpToNearestTenAndDropAddThree(p.hp) - 3)
    val str = convertInitialStat(p.attack)
    val dex = convertInitialStat(p.speed)
    // Con being too low means too short of battles
    val con  = Math.max(convertInitialStat((p.defense + p.hp) / 2), 8)
    val int = convertInitialStat(p.spAttack)
    val wis = convertInitialStat(Math.max(p.spAttack, p.spDefense))
    val statMeanOrMin = Math.max((str + dex + con + int + wis) / 5, 6)
    val cha = trendTowardsMean(chaBase(statMeanOrMin, PokemonList.typesForPokemon.getOrElse(name, Seq.empty[Types.Value])))
    val maybeDndEvoLevel = maybeEvoLevel.map(l => Math.ceil(l.toDouble / 5d))
    val types = PokemonList.typesForPokemonKey.getOrElse(PokemonList.nameKey(name), Seq())
    val dndMoves = pokemonToDndMoves(p.moves)
    val rolledMoves = rollForMoves(dndMoves, level)
    val maybeShinyMove =
      if (shinyMove) Some(rollForRandomShinyMove(rolledMoves, types))
      else None

    val baseStats =
      DnDStats(
        name = name,
        level = level,
        challengeRating = cr,
        nextEvolutionLevel = maybeDndEvoLevel.map(_.toInt),
        hitDice = hitDice,
        rolledMaxHp = maxHp(hitDice, level, con, maxPossible = false),
        maxPossibleHp = maxHp(hitDice, level, con, maxPossible = true),
        // Defense should be weighted heavier when considering AC
        armorClass = Math.max(10, Math.max(dex, Math.max(con, convertInitialStat(p.defense)))),
        strength = str,
        dexterity = dex,
        constitution = con,
        intelligence = int,
        wisdom = wis,
        charisma = cha,
        movementSpeed = movementSpeed(dex),
        availableMoves = dndMoves,
        rolledMoves = rolledMoves ++ maybeShinyMove.toSeq)

    if (level != 1 || autoUpLevel) autoLevelUp(baseStats, level)
    else baseStats
  }
}

