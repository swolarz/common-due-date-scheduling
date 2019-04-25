import java.io.PrintStream

import data.SchedulingInstance
import data.export.SchedulingExporter
import data.loader.{DataLoader, UpperBoundsLoader}
import scheduler.genetic.GeneticJobScheduler
import scheduler.{DynamicJobScheduler, FastJobScheduler, JobScheduler}

object SchedulerApplication {
  val debug = false

  def main(args: Array[String]): Unit = {
    if (args.length < 3) {
      println("Usage: schedule <n> <k> <h>")
      System.exit(1)
    }

    try {
      val n: Int = args(0).toInt
      val k: Int = args(1).toInt
      val h: Double = args(2).toDouble

      println(s"Scheduling operation with parameters: n = $n, k = $k, h = $h")

      val dataLoader: DataLoader = new DataLoader(debug)
      val instance: SchedulingInstance = dataLoader.loadInstance(n, k, h)

      if (debug)
        println(instance)

      println(s"Total jobs processing time = ${instance.getTotalProcessingTime}")

      val scheduler: JobScheduler = new GeneticJobScheduler
      val scheduling = scheduler.schedule(instance)

      val upperBound = new UpperBoundsLoader().getUpperBound(n, k, h)
      val penalty = scheduling.getPenalty
      val error = (penalty - upperBound).toDouble / upperBound

      println(f"Result scheduling penalty = $penalty, r = ${scheduling.offset}, upperBound = $upperBound error = ${error * 100}%.3f%%")

      val exporter = new SchedulingExporter
      exporter.export(scheduling)

    } catch {
      case e: Exception =>
        println(e.getMessage)
        e.printStackTrace()
    }
  }
}