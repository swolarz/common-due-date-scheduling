import data.SchedulingSummary
import data.export.SummaryTableExporter
import data.loader.{DataLoader, UpperBoundsLoader}
import data.util.InstancesInfo._
import data.validation.SchedulingValidator
import scheduler.{DynamicJobScheduler, FastJobScheduler, GeneticJobScheduler, JobScheduler}

object SummaryApplication {

  def main(args: Array[String]): Unit = {
    val dataLoader = new DataLoader
    val upperBoundsLoader = new UpperBoundsLoader
    val scheduler: JobScheduler = new GeneticJobScheduler

    val tableExporter = new SummaryTableExporter
    val schedulingValidator = new SchedulingValidator

    try {
      Array(50) foreach { n =>
        hValues foreach { h =>
          kValues foreach { k =>
            val instance = dataLoader.loadInstance(n, k, h)
            val upperBound = upperBoundsLoader.getUpperBound(n, k, h)

            val startNano = System.nanoTime()
            val scheduling = scheduler.schedule(instance)
            val endNano = System.nanoTime()

            val summary = SchedulingSummary(n, h, k, upperBound, scheduling.getPenalty, (endNano - startNano).toDouble / 1000000)
            tableExporter.addSummary(summary)

            val correct = schedulingValidator.validate(scheduling)

            print(f"Scheduled instance with params:\tn = $n%5d\th = $h%4.1f \tk = $k%3d")
            println(f"  |  r = ${scheduling.offset}%4d\ttime = ${summary.executionMillis}%6.0f ms \terror = ${summary.errorRate * 100}%8.3f \t${if (correct) "OK" else "WRONG"}")

            summary.executionMillis
          }
        }
      }

      tableExporter.exportTable()

    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
  }
}
