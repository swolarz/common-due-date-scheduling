package data.export

import java.io.PrintStream

import data.util.DataUtils.autoClose
import data.{JobScheduling, SchedulingInstance}

class SchedulingExporter(private var exportPath: String = "./results") {
  if (!exportPath.endsWith("/"))
    exportPath += "/"

  def export(scheduling: JobScheduling): Unit = {
    val exportPath = getExportFilePath(scheduling.instance)

    autoClose(new PrintStream(exportPath)) { output =>
      writeExportContent(output, scheduling)
    }
  }

  private def getExportFilePath(instance: SchedulingInstance): String = {
    val n = instance.jobs.length
    val k = instance.k
    val h = instance.h
    val hf = (h * 10).round % 10

    s"${exportPath}n${n}k${k}h$hf.txt"
  }

  private def writeExportContent(output: PrintStream, scheduling: JobScheduling): Unit = {
    val penalty = scheduling.getPenalty
    val h = scheduling.instance.h
    val r = scheduling.offset
    val jobIds = () => scheduling.orderedJobs.map(_.id).mkString(" ")

    output.println(s"$penalty $h $r ${jobIds()}")
  }
}
