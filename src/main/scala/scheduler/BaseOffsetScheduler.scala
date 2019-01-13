package scheduler

import data.{JobScheduling, ScheduledJob, SchedulingInstance}

import scala.collection.mutable.ArrayBuffer

abstract class BaseOffsetScheduler extends JobScheduler {
  def makeSchedulingWithOffset(instance: SchedulingInstance, orderedJobs: Array[ScheduledJob]): JobScheduling = {
    val deadline = instance.getDeadline
    val offset = findOptimalOffset(orderedJobs, deadline)

    new JobScheduling(instance, orderedJobs, offset)
  }

  def findOptimalOffset(scheduledJobs: Array[ScheduledJob], deadline: Int): Int = {
    var split = 0
    var time = 0
    var beforeWeights = 0
    var afterWeights = 0
    var offset = 0

    while (time + scheduledJobs(split).p < deadline) {
      time += scheduledJobs(split).p
      beforeWeights += scheduledJobs(split).a
      split += 1
    }
    afterWeights = scheduledJobs.drop(split).map(_.b).sum
    split -= 1

    while (split >= 0 && beforeWeights >= afterWeights) {
      offset = deadline - time
      afterWeights += scheduledJobs(split).b
      beforeWeights -= scheduledJobs(split).a
      time -= scheduledJobs(split).p
      split -= 1
    }

    offset
  }
}
