package scheduler
import data.{JobScheduling, ScheduledJob, SchedulingInstance}

import scala.collection.mutable.ArrayBuffer

class DynamicJobScheduler extends BaseOffsetScheduler {
  private case class IncrementalPartition(var dp: Int = Int.MaxValue, var prev: Int = -1, var length: Int = 0, var lastJobId: Int = -1)

  override def schedule(instance: SchedulingInstance): JobScheduling = {
    val d = instance.getDeadline
    val part = Array.fill[IncrementalPartition](instance.jobs.length + 1)(IncrementalPartition())

    val jobs: Array[ScheduledJob] = instance.jobs.sortBy(job => job.p.toDouble / job.a)

    val dummyJob = ScheduledJob(-1, 0, 0, 0)
    val aOrdered: Array[ScheduledJob] = Array(dummyJob) ++ jobs
    val bOrdered = instance.jobs.sortBy(job => job.p.toDouble / job.b)

    for ((job, i) <- aOrdered.zipWithIndex.drop(1)) {
      var best = IncrementalPartition(Int.MaxValue, -1, -1, -1)

      for ((prevjob, j) <- aOrdered.take(i).zipWithIndex) {

        if (part(j).length + job.p <= d) {

          part(i).lastJobId = job.id
          part(i).prev = j
          part(i).length = part(j).length + job.p

          val prevList = previousJobs(part, i)

          part(i).dp = calculateDp(aOrdered.drop(0), bOrdered, prevList.toSet, d)

          if (part(i).dp < best.dp)
            best = part(i)
        }
      }

      part(i) = best
    }

    var bestIdx = 0

    for ((partition, index) <- part.zipWithIndex) {
      if (partition.dp < part(bestIdx).dp) {
        bestIdx = index
      }
    }

    val prevList = previousJobs(part, bestIdx)
    val previous = prevList.toSet
    val beforeJobs = prevList.flatMap(instance.jobsDict.get)
    val afterJobs = bOrdered.filterNot(job => previous.contains(job.id))

    val scheduledJobs = beforeJobs ++ afterJobs

    makeSchedulingWithOffset(instance, scheduledJobs)
  }

  private def calculateDp(aOrdered: Array[ScheduledJob], bOrdered: Array[ScheduledJob], leftSet: Set[Int], deadline: Int): Int = {
    var pen = 0
    var lTime = 0
    var rTime = 0
    var aWeights = 0
    var bWeights = 0

    var firstRightJob: ScheduledJob = null

    for (job <- aOrdered) {
      if (leftSet contains job.id) {
        aWeights += job.a
        pen += job.a * lTime
        lTime += job.p
      }
    }

    for (job <- bOrdered) {
      if (!(leftSet contains job.id)) {
        if (firstRightJob == null)
          firstRightJob = job

        bWeights += job.b
        rTime += job.p
        pen += job.b * rTime
      }
    }

    val maxShift = math.min(deadline - lTime, firstRightJob.p)

    if (aWeights < bWeights)
      pen -= (bWeights - aWeights) * maxShift

    pen
  }

  private def previousJobs(part: Array[IncrementalPartition], pos: Int): Array[Int] = {
    val prevList = new ArrayBuffer[Int]
    var pt = pos

    while (pt > 0) {
      prevList.append(part(pt).lastJobId)
      pt = part(pt).prev
    }

    prevList.toArray
  }
}
