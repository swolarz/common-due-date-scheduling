import data.loader.{DataLoader, JobSchedulingLoader}

object ValidatorApplication {

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: validator <resfile>")
      System.exit(1)
    }

    try {
      val resFilepath = args(0)

      val dataLoader = new DataLoader
      val schedulingLoader = new JobSchedulingLoader(dataLoader)

      val (scheduling, externalPenalty) = schedulingLoader.loadScheduling(resFilepath)

      if (scheduling.getPenalty != externalPenalty)
        println(s"Calculated penalty does not match the penalty in the solution file: expected ${scheduling.getPenalty} and got $externalPenalty")
      else
        println(s"Loaded job scheduling is correct. Penalty = $externalPenalty")

    } catch {
      case e: Exception =>
        println(e.getMessage)
        e.printStackTrace()
    }
  }
}
