package data.validation

import data.JobScheduling

class SchedulingValidator {
  def validate(scheduling: JobScheduling): Boolean = {
    val jobIds = scheduling.orderedJobs.map(_.id)

    val n = jobIds.length
    val offsetCondition = scheduling.offset >= 0
    val lengthCondition = scheduling.instance.jobs.length == n
    val uniqueCondition = jobIds.toSet.size == n
    val rangeCondition = jobIds.forall(job => 0 <= job && job < n)

    return offsetCondition && lengthCondition && uniqueCondition && rangeCondition
  }
}
