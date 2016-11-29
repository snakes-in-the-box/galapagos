import scala.collection.mutable.{HashMap, ListBuffer}
import DataPipeline._
import Tree._

import scala.util.Random

object Driver{

  val dirPath = "C:/Users/Brent/Documents/School/DataPrac/FinalData/"

  def importData(file: String): List[HashMap[String, Double]] = {
    DataPipeline.readFile(dirPath + file)
  }

  def run(pop: ListBuffer[Tree], data: List[HashMap[Double, String]], tourSize: Int, maxGens: Int, ran: Random): Tree = {

  }

  def main(args:Array[String]):Unit={



  }
}
