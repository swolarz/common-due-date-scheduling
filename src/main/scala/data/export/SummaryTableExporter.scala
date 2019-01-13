package data.export

import java.io.PrintStream

import data.SchedulingSummary
import data.util.DataUtils.autoClose

import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode

class SummaryTableExporter(private var exportPath: String = "./results") {
  if (!exportPath.endsWith("/"))
    exportPath += "/"

  private val summaries = new ArrayBuffer[SchedulingSummary]

  def addSummary(summary: SchedulingSummary): Unit = {
    summaries.append(summary)
  }

  def exportTable(): Unit = {

    autoClose(new PrintStream(exportPath + "summary-table.csv")) { output =>
      summaries foreach { summary =>
        val formattedErrorRate = BigDecimal(summary.errorRate).setScale(4, RoundingMode.HALF_UP).toString
        val formattedExecutionTime = BigDecimal(summary.executionMillis).setScale(6, RoundingMode.HALF_UP).toString
        output.println(s"${summary.n};${summary.h};${summary.k};${summary.upperBound};${summary.schedulingPenalty};${formattedErrorRate};${formattedExecutionTime}")
      }
    }
  }
}
