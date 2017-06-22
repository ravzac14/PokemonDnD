package models

import data.Types

// Name|Type|ActionType|Range(in feet)|Duration|CastingTime|RechargeValues(d6)|AttackAbilityMod|Description
case class DnDMove(
  level: Option[Int],
  name: String,
  types: Seq[Types.Value],
  moveType: Seq[MoveType.Value],
  range: Option[Range],
  duration: Option[String],
  castingTime: Seq[ActionType.Value],
  rechargeValues: Seq[String],
  associatedAbilities: Seq[DnDAbility.Value],
  description: String) {
  val levelString = level.map(_.toString).getOrElse("N/A")
  val typesString = if (types.nonEmpty) types.mkString(", ") else "N/A"
  val rangeString = range match {
    case Some(r) if r.isTouch => "Touch"
    case Some(Range(_, Some(min), Some(max))) => s"$min/$max"
    case Some(Range(_, Some(min), _)) => s"$min"
    case _ => "N/A"
  }
  val moveTypeString = if (moveType.nonEmpty) moveType.map(_.toString.capitalize).mkString(", ")  else "N/A"
  val durationString = duration.map(_.capitalize).getOrElse("N/A")
  val castingTimeString = if (castingTime.nonEmpty) castingTime.map(_.toString.capitalize).mkString(", ") else "N/A"
  val rechargeValuesString = if (rechargeValues.nonEmpty) rechargeValues.mkString(", ") else "N/A"
  val associatedAbilitiesString = if (associatedAbilities.nonEmpty) associatedAbilities.map(_.toString.capitalize).mkString(", ") else "N/A"

  def nameKey = StatTransformers.stringKey(name)
  def simplePrint = s"level: ${level.getOrElse(1)} -- ${name.capitalize}"
  def prettyPrint =
    s"""%%%%%%%%%%%%%%%%%%%%%%%%%%%%% $name %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        |Level: $levelString
        |Type(s): $typesString
        |Move Type(s): $moveTypeString""".stripMargin

  def verbosePrint =
    s"""$prettyPrint
       |Range: $rangeString
       |Duration: $durationString
       |Casting Time(s): $castingTimeString
       |Recharge Value(s): $rechargeValuesString
       |Associated Abilities: $associatedAbilitiesString
       |Desc: $description""".stripMargin

  def isDefault = nameKey == "tackle" || nameKey == "scratch" || nameKey == "pound"
}

case class Range(isTouch: Boolean, min: Option[Int]= None, max: Option[Int]= None)

object MoveType extends Enumeration {
  val Spell, MeleeAttack, RangedAttack, RoomSpell, Trait = Value

  def fromPsvRowItem(s: String): Seq[MoveType.Value] =
    s.trim.toLowerCase.replaceAll(" ","").split(",").map(fromString)

  def fromString(s: String) = s.trim.toLowerCase.replaceAll(" ","") match {
    case "spell" => Spell
    case "meleeattack" => MeleeAttack
    case "rangedattack" => RangedAttack
    case "roomspell" => RoomSpell
    case "trait" => Trait
    case e => throw new Exception(s"Unknown MoveType [$e]!")
  }
}
object ActionType extends Enumeration {
  val Full, Bonus, Reaction = Value

  def fromPsvRowItem(s: String): Seq[ActionType.Value] =
    s.trim.toLowerCase.replaceAll(" ","").split(",").map(fromString)

  def fromString(s: String) = s.trim.toLowerCase.replaceAll(" ","") match {
    case "action" => Full
    case "bonusaction" => Bonus
    case "reaction" => Reaction
    case e => throw new Exception(s"Unknown ActionType [$e]!")
  }
}

object DnDMoveBuilders {

  // Name|Type|ActionType|Range(in feet)|Duration|CastingTime|RechargeValues(d6)|AttackAbilityMod|Description
  def fromPsvRow(row: String): DnDMove = {
    def isEmpty(s: String) = s.isEmpty || s == "--"
    val rowItems = row.split('|').map(_.trim.toLowerCase)
    assert(rowItems.length == 9, s"Found malformed Move row [$row]")
    val name = if (!isEmpty(rowItems(0))) rowItems(0) else throw new MissingRequiredField("Name", row)
    val types = if (!isEmpty(rowItems(1))) Types.fromPsvRowItem(rowItems(1)) else Seq.empty
    val actionType = if (!isEmpty(rowItems(2))) MoveType.fromPsvRowItem(rowItems(2)) else throw new MissingRequiredField("ActionType", row)
    val range = if (!isEmpty(rowItems(3))) Some(rangeFromString(rowItems(3))) else None
    val duration = if (!isEmpty(rowItems(4))) Some(rowItems(4)) else None
    val castingTime = if (!isEmpty(rowItems(5))) ActionType.fromPsvRowItem(rowItems(5)) else Seq.empty
    val rechargeValues = if (!isEmpty(rowItems(6))) rechargeValuesFromString(rowItems(6)) else Seq.empty
    val attackMods = if (!isEmpty(rowItems(7))) DnDAbility.fromPsvRowItem(rowItems(7)) else Seq.empty
    val description = if (!isEmpty(rowItems(8))) rowItems(8) else throw new MissingRequiredField("Description", row)
    DnDMove(None, name, types, actionType, range, duration, castingTime, rechargeValues, attackMods, description)
  }

  class MissingRequiredField(
    fieldName: String,
    row: String)
    extends Exception(s"Could not find required field [$fieldName] for row [$row]")

  private def rangeFromString(s: String): Range = {
    if (s == "touch") Range(true)
    else {
      s.split("/") match {
        case Array(min, max) => Range(false, Some(min.toInt), Some(max.toInt))
        case Array(min) => Range(false, Some(min.toInt))
        case _ => throw new Exception(s"Malformed range [$s]!")
      }
    }
  }

  private def rechargeValuesFromString(s: String): Seq[String] = s.split(",")
}
