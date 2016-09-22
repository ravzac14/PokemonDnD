package models

object Dice {
  val D20Average = 11
  val D12Average = 7
  val D10Average = 6
  val D8Average = 5
  val D6Average = 4
  val D4Average = 3

  private val r = scala.util.Random

  def D20Roll = r.nextInt(19) + 1
  def D12Roll = r.nextInt(11) + 1
  def D10Roll = r.nextInt(9) + 1
  def D8Roll = r.nextInt(8) + 1
  def D6Roll = r.nextInt(5) + 1
  def D4Roll = r.nextInt(3) + 1

  def D20RollOrAverage = Math.max(D20Roll,D20Average)
  def D12RollOrAverage = Math.max(D12Roll, D12Average)
  def D10RollOrAverage = Math.max(D10Roll, D10Average)
  def D8RollOrAverage = Math.max(D8Roll, D8Average)
  def D6RollOrAverage = Math.max(D6Roll, D6Average)
  def D4RollOrAverage = Math.max(D4Roll, D4Average)
}