/*
 * Copyright 2020-2022 Greg von Nessi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
