package scheduler.genetic

import data.SchedulingInstance

class GenerationProgress(val instance: SchedulingInstance, val upperBound: Int, val totalMillis: Long) {
  private var infoLength = 0
  private var bestPenalty = Int.MaxValue

  var _penalty: Int = 0
  def penalty: Int = _penalty
  def penalty_= (value: Int): Unit = {
    _penalty = value
    bestPenalty = math.min(bestPenalty, _penalty)
  }

  var generation: Int = 0
  var passedMillis: Long = 0
  var populationSize: Int = 0

  def update(generation: Int, penalty: Int, millis: Long, populationSize: Int): Unit = {
    this.generation = generation
    this.penalty = penalty
    this.passedMillis = millis
    this.populationSize = populationSize
  }

  def display(): Unit = {
    val currentError = ((penalty - upperBound).toDouble / upperBound) * 100
    val bestError = ((bestPenalty - upperBound).toDouble / upperBound) * 100
    val remainingMillis = math.max(totalMillis - passedMillis, 0)

    val instanceInfo = f"[n = ${instance.jobs.length}%5d   |   h = ${instance.h}%.1f   |   k = ${instance.k}%2d]  "
    val generationInfo = f"Generation $generation:  "
    val currentErrorInfo = f"current error = $currentError%4.3f%%"
    val bestErrorInfo = f"best error = $bestError%4.3f%%"
    val timeInfo = f"time = $passedMillis%6d ms (remaining = $remainingMillis%6d ms)"
    val populationInfo = f"population = $populationSize"
    val penaltyInfo = f"penalty = $penalty%8d; best = $bestPenalty%8d"
    val displayInfo = Array(timeInfo, populationInfo, currentErrorInfo, bestErrorInfo, penaltyInfo).mkString(instanceInfo + generationInfo, " | ", "")

    infoLength = displayInfo.length

    clearDisplay()
    print("\r" + displayInfo)
  }

  def clearDisplay(): Unit = {
    print("\r" + " " * infoLength)
  }

  def finish(): Unit = {
    clearDisplay()
    print("\r")
  }
}
