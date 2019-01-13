package data.loader

import java.io.InputStream
import java.util.Scanner

import data.util.DataUtils.autoClose
import data.{ScheduledJob, SchedulingInstance}

import scala.collection.mutable.ArrayBuffer


class DataLoader(var verbose: Boolean = false) {

  def loadInstance(n: Int, k: Int, h: Double): SchedulingInstance = {

    val dataFilepath = s"/benchmark/sch$n.txt"
    val inputStream: InputStream = getClass.getResourceAsStream(dataFilepath)

    if (inputStream == null)
      throw new IllegalArgumentException(s"No such instance with n = $n")

    val scanner: Scanner = new Scanner(inputStream)
    val jobsArr = new ArrayBuffer[ScheduledJob]

    val ignoreJob = (id: Int, p: Int, a: Int, b: Int) => {}
    val addJob = (id: Int, p: Int, a: Int, b: Int) => jobsArr.append(ScheduledJob(id, p, a, b))

    autoClose(scanner) { scanner =>
      val problems = scanner.nextInt

      if (verbose)
        println(s"Found $problems problems.")

      1 to problems foreach { problem =>

        val jobs = scanner.nextInt
        val jobLambda = if (problem == k) addJob else ignoreJob

        if (verbose)
          println(s"Found $jobs jobs in problem $problem.")

        0 until jobs foreach { jobId =>
          val p = scanner.nextInt
          val a = scanner.nextInt
          val b = scanner.nextInt

          if (verbose)
            println(s"Parsing job with id = $jobId, p = $p, a = $a, b = $b...")

          jobLambda(jobId, p, a, b)
        }
      }
    }

    if (verbose)
      println(s"Parsed ${jobsArr.size} jobs.")

    new SchedulingInstance(jobsArr.toArray, h, k)
  }
}
