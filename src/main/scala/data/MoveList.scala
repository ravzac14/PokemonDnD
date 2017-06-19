package data

import models.{DnDMoveBuilders, DnDMove}

import scala.io.Source

object MoveList {
  private val MovesFile = "/home/zack/Github/PokemonDnD/moves.psv"
  // Name|Type|ActionType|Range(in feet)|Duration|CastingTime|RechargeValues(d6)|AttackAbilityMod|Description
  lazy val allMoves: Seq[DnDMove] =
    Source
      .fromFile(MovesFile)
      .getLines
      .toSeq
      .tail // Ditch the header
      .map(DnDMoveBuilders.fromPsvRow)

  def default = allMoves.find(_.nameKey == "pound").get
}
