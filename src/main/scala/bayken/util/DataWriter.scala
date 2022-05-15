package ai.entrolution
package bayken.util

import com.opencsv.CSVWriter

import java.io.{BufferedWriter, File, FileWriter}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Try}

object DataWriter {

  def writeDatFile(
      input: List[((Double, Double), Double)],
      fileName: String
  ): Unit = {
    val rawLines = input
      .groupBy(_._1._1)
      .toList
      .sortBy(_._1)
      .map { dataBlock =>
        dataBlock._2
          .sortBy(_._1._2)
          .map(i => s"${i._1._1} ${i._1._2} ${i._2}\n") ++ List("\n")
      }
      .reduce(_ ++ _)

    val file = new File(fileName)
    val bw   = new BufferedWriter(new FileWriter(file))
    for (line <- rawLines)
      bw.write(line)
    bw.close()
  }

  def writeCsvFile(
      header: List[String],
      rows: List[List[String]],
      fileName: String
  ): Try[Unit] =
    Try(new CSVWriter(new BufferedWriter(new FileWriter(fileName))))
      .flatMap((csvWriter: CSVWriter) =>
        Try {
          csvWriter.writeAll(
            (header +: rows).map(_.toArray).asJava
          )
          csvWriter.close()
        } match {
          case f @ Failure(_) =>
            Try(csvWriter.close()).recoverWith { case _ =>
              f
            }
          case success =>
            success
        }
      )
}
