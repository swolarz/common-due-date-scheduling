package data.loader

import java.io.{File, FileNotFoundException}
import java.util.{Locale, Scanner}

import data.util.DataUtils.autoClose
import data.JobScheduling

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class JobSchedulingLoader(dataLoader: DataLoader) {

  def loadScheduling(schedulingFilepath: String): (JobScheduling, Int) = {
    val schedulingFile = new File(schedulingFilepath)

    if (!schedulingFile.isFile || !schedulingFile.exists)
      throw new FileNotFoundException(s"Specified file not found: $schedulingFilepath")

    val regexp = "n(\\d+)k(\\d+)h(\\d+).txt".r
    val schedulingFilename = schedulingFile.getName

    schedulingFile.getName match {
      case regexp(n, k, h) => loadScheduling(schedulingFile, n.toInt, k.toInt, h.toDouble / 10)
      case _ => throw new IllegalArgumentException(s"Filename does not conform to the pattern n<N>k<K>h<H>.txt: ${schedulingFile.getName}")
    }
  }

  private def loadScheduling(schedulingFile: File, n: Int, k: Int, h: Double): (JobScheduling, Int) = {
    val instance = dataLoader.loadInstance(n, k, h)
    val jobsDict = (instance.jobs map (job => job.id -> job)).toMap

    val scanner = new Scanner(schedulingFile).useLocale(Locale.US)
    val jobIds = new ArrayBuffer[Int]
    val jobIdSet = new mutable.HashSet[Int]
    var penalty = -1
    var offset = -1
    var rn = 0

    autoClose(scanner) { scanner =>
      penalty = scanner.nextInt
      scanner.nextDouble
      offset = scanner.nextInt

      if (offset < 0)
        throw new IllegalStateException("Offset mustn't be a negative value.")

      while (scanner.hasNextInt) {
        val jobId = scanner.nextInt

        if (!(0 <= jobId && jobId < n))
          throw new IllegalStateException(s"Job id must fit the interval [0, n - 1] (n = $n).")

        if (jobIdSet.contains(jobId))
          throw new IllegalStateException(s"Job with id = $jobId has already occurred in the sequence.")

        rn += 1
        if (rn > n)
          throw new IllegalStateException(s"Loaded more than n ($n) job ids.")

        jobIds.append(jobId)
        jobIdSet.add(jobId)
      }
    }

    if (rn < n)
      throw new IllegalStateException(s"Loaded less than n ($n) job ids.")

    val orderedJobs = jobIds.map(jobId => jobsDict(jobId)).toArray
    (new JobScheduling(instance, orderedJobs, offset), penalty)
  }
}
