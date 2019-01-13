package data

class JobScheduling(val instance: SchedulingInstance, val orderedJobs: Array[ScheduledJob], val offset: Int) {

  def getPenalty: Int = {
    val deadline = instance.getDeadline
    JobScheduling.calculatePenalty(orderedJobs, deadline, offset)
  }
}

object JobScheduling {
  def calculatePenalty(orderedJobs: Array[ScheduledJob], deadline: Int, offset: Int): Int = {
    var lastFinished = offset
    var penalty = 0

    orderedJobs foreach { job =>
      val finishTime = lastFinished + job.p
      val shift = (deadline - finishTime).abs

      penalty += shift * (if (finishTime < deadline) job.a else job.b)
      lastFinished = finishTime
    }

    penalty
  }
}
