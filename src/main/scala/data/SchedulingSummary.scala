package data

case class SchedulingSummary(n: Int, h: Double, k: Int, r: Int, upperBound: Int, schedulingPenalty: Int, executionMillis: Double) {
  val errorRate: Double = (schedulingPenalty - upperBound).toDouble / upperBound
}
