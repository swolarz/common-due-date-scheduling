package data

class SchedulingInstance(val jobs: Array[ScheduledJob], val h: Double, val k: Int) {
  val jobsDict: Map[Int, ScheduledJob] = jobs.map(job => job.id -> job).toMap

  def getTotalProcessingTime: Int = jobs.map(_.p).sum
  def getDeadline: Int = (h * getTotalProcessingTime).floor.toInt

  override def toString: String = {
    val sb = new StringBuilder()

    sb.append(s"Number of jobs = ${jobs.length}:")
    jobs foreach { job =>
      sb.append(s"\n\t${job.id}: processing time = ${job.p}, earliness penalty = ${job.a}, tardiness penalty = ${job.b}")
    }

    sb.toString()
  }
}
