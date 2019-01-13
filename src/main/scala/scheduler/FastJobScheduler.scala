package scheduler

import data.{JobScheduling, ScheduledJob, SchedulingInstance}

import scala.collection.mutable.ArrayBuffer

class FastJobScheduler extends BaseOffsetScheduler {

  override def schedule(instance: SchedulingInstance): JobScheduling = {
    val d: Int = instance.getDeadline
    val sortedJobs = instance.jobs sortBy (job => job.a - job.b)
    var scheduledJobs = new ArrayBuffer[ScheduledJob]
    var toBeScheduledJobs = new ArrayBuffer[ScheduledJob]

    var f = 0

    sortedJobs foreach { job =>
      if (f + job.p <= d) {
        scheduledJobs.append(job)
        f += job.p
      } else {
        toBeScheduledJobs.append(job)
      }
    }

    scheduledJobs = scheduledJobs.sortBy(job => job.p.toDouble / job.a).reverse
    toBeScheduledJobs = toBeScheduledJobs.sortBy(job => job.p.toDouble / job.b)

    scheduledJobs.appendAll(toBeScheduledJobs)

    makeSchedulingWithOffset(instance, scheduledJobs.toArray)
  }
}
