import scala.collection.mutable.{HashMap, ListBuffer}
import DataPipeline._
import Tree._

import scala.collection.mutable
import scala.util.Random

import org.apache.spark._
import org.apache.spark.rdd.RDD
import org.apache.hadoop.mapred._

object Driver{

  val sparkConf = new SparkConf().setAppName("BlkFish")
  val sc = new SparkContext(sparkConf)

  val dirPath = "C:/Users/Brent/Documents/School/DataPrac/FinalData/15_min/"

  val tournamentSize = 2

  val ran = new Random(System.currentTimeMillis())

  val populationSize = 5

  val maxDepth = 3

  val maxGenerations = 2

  def importFile(year: Int): List[HashMap[String, Double]] = {
    DataPipeline.readFile(dirPath + year.toString + "-1.csv") ++: DataPipeline.readFile(dirPath + year.toString + "-2.csv")
  }

  def importData(start: Int, end: Int): List[HashMap[String, Double]] = {
    if (start <= end) {
      importFile(start) ++: importData(start+1, end)
    }
    else List[HashMap[String, Double]]()
  }

  def findBest(pop: ListBuffer[Tree], data: List[HashMap[String, Double]]): Tree = {
    pop.foldLeft(pop.head) {
      (t1: Tree, t2: Tree) =>
        val f1 = findAverageFitness(t1, data)
        val f2 = findAverageFitness(t2, data)
        if ((f1 < f2 && !f1.isNaN) || f2.isNaN) t1
        else t2
    }
  }

  def runAux(pop: ListBuffer[Tree], data: List[HashMap[String, Double]], tourSize: Int, maxGen: Int, curGen: Int, ran: Random): Tree = {
    println(curGen)
    if (curGen <= maxGen) {
      val nextGen = nextGeneration(pop, tourSize, data, ran)
      runAux(nextGen, data, tourSize, maxGen, curGen+1, ran)
    }
    else findBest(pop, data)
  }

  def run(pop: ListBuffer[Tree], maxDepth: Int, data: List[HashMap[String, Double]], tourSize: Int, maxGen: Int, ran: Random): Tree = {
    runAux(pop, data, tourSize, maxGen, 0, ran)
  }

  def main(args:Array[String]):Unit={

    val data = importData(2016, 2016).filter( inst => inst.nonEmpty )
    val pop = initializePopulation(populationSize, maxDepth, ran)
    val popRDD = sc.parallelize(pop.sliding(pop.size/4).toSeq)
    val runRDD = popRDD.map( pop => run(pop, maxDepth, data, tournamentSize, maxGenerations, ran))
    val result = runRDD.reduce({ (t1, t2) =>
      val f1 = findAverageFitness(t1, data)
      val f2 = findAverageFitness(t2, data)
      if (f1 < f2 && !f1.isNaN) t1
      else t2
    })
    println(Tree.toString(result))
    println(findAverageFitness(result, data))
  }
}
