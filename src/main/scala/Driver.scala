import scala.collection.mutable.{HashMap, ListBuffer}
import DataPipeline._
import Tree._

import scala.util.Random

object Driver{

  val dirPath = "C:/Users/Brent/Documents/School/DataPrac/FinalData/15_min/"

  val tournamentSize = 2

  val ran = new Random(System.currentTimeMillis())

  val populationSize = 10

  val maxDepth = 3

  val maxGenerations = 200

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
        if (((f1 < f2 && f1 != Double.NaN) || f2 == Double.NaN) || f2 == 0 ) t1
        else t2
    }
  }

  def runAux(pop: ListBuffer[Tree], data: List[HashMap[String, Double]], tourSize: Int, maxGen: Int, curGen: Int, ran: Random): Tree = {
    println(curGen)
    /*pop.foldLeft(pop.head)( (t1, t2) => { println(Tree.toString(t1))
                                        println(Tree.findAverageFitness(t1, data))
                                        t2})*/
    if (curGen <= maxGen) {
      if (curGen % 25 == 0) {
        val bb = findBest(pop, data)
        println("\nBest:\n" ++ Tree.toString(bb))
        println(Tree.findAverageFitness(bb, data).toString ++ "\n")
      }
      val nextGen: ListBuffer[Tree] = nextGeneration(pop, tourSize, data, ran)
      runAux(nextGen, data, tourSize, maxGen, curGen+1, ran)
    }
    else {
      findBest(pop, data)
    }
  }

  def run(popSize: Int, maxDepth: Int, data: List[HashMap[String, Double]], tourSize: Int, maxGen: Int, ran: Random): Tree = {
    val pop = initializePopulation(popSize, maxDepth, ran)
    runAux(pop, data, tourSize, maxGen, 0, ran)
  }

  def main(args:Array[String]):Unit={

    val data = importData(2016, 2016).filter( inst => !inst.isEmpty )
    val result = run(populationSize, maxDepth, data, tournamentSize, maxGenerations, ran)
    println(Tree.toString(result))
    println(findAverageFitness(result, data))
  }
}
