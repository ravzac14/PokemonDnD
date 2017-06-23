package data

import models.{DnDMoveBuilders, DnDMove}

import scala.collection.immutable.Iterable
import scala.io.Source

object MoveList {
  private val MovesFile = "/home/zack/Github/PokemonDnD/moves.psv"

  lazy val movesByTypes: Map[Seq[Types.Value], Seq[DnDMove]] = allMoves.groupBy(_.types)

  def movesForType(t: Types.Value): Seq[DnDMove] =
    movesByTypes
      .collect { case (types, moves) if types.contains(t) => moves }
      .flatten
      .toSeq

  // Name|Type|ActionType|Range(in feet)|Duration|CastingTime|RechargeValues(d6)|AttackAbilityMod|Description
  lazy val allMoves: Seq[DnDMove] =
    Source
      .fromFile(MovesFile)
      .getLines
      .toSeq
      .tail // Ditch the header
      .map(DnDMoveBuilders.fromPsvRow)

  def default =
    allMoves
      .find(_.nameKey == "pound")
      .getOrElse(throw new Exception("Could not find Pound lol."))
}
