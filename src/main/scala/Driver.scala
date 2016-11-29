import scala.collection.mutable.{HashMap, ListBuffer}
import DataPipeline._
import Tree._

import scala.collection.mutable
import scala.util.Random

object Driver{

  val dirPath = "C:/Users/Brent/Documents/School/DataPrac/FinalData/15_min/"

  def importFile(year: Int): List[HashMap[String, Double]] = {
    DataPipeline.readFile(dirPath + year.toString + "-1.csv") ++: DataPipeline.readFile(dirPath + year.toString + "-2.csv")
  }

  def importData(start: Int, end: Int): List[HashMap[String, Double]] = {
    if (start <= end) {
      importFile(start) ++: importData(start+1, end)
    }
    else List[HashMap[String, Double]]()
  }

  def run(pop: ListBuffer[Tree], data: List[HashMap[Double, String]], tourSize: Int, maxGens: Int, ran: Random): Tree = {

  }

  def main(args:Array[String]):Unit={



  }
}
