import Tree.Tree

import scala.collection.mutable
import scala.util.Random
import org.apache.spark._
import org.apache.spark.rdd.RDD
import org.apache.hadoop.mapred._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object Driver{

  val sparkConf = new SparkConf().setAppName("BlkFish")
  val sc = new SparkContext(sparkConf)

  val dirPath = "C:/Users/Brent/Documents/School/DataPrac/FinalData/15_min/"

  val tournamentSize = 3

  val ran = new Random(System.currentTimeMillis())

  val populationSize = 20

  val maxDepth = 5

  val maxGenerations = 100

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
        val f1 = Tree.findAverageFitness(t1, data)
        val f2 = Tree.findAverageFitness(t2, data)
        //println("\nt1: " ++ Tree.toString(t1))
        //println("f1: " ++ f1.toString)
        //println("t2: " ++ Tree.toString(t2))
        //println("f2: " ++ f2.toString ++ "\n")
        //println("f1 NaN: " ++ (f1.isNaN).toString)
        //println("f2 NaN: " ++ (f2.isNaN).toString)
        if ((f1 < f2 && !f1.isNaN && f1 != 0.0) || f2.isNaN) t1
        else t2
    }
  }

  def runAux(pop: ListBuffer[Tree], data: List[HashMap[String, Double]], tourSize: Int, maxGen: Int, curGen: Int, ran: Random): Tree = {
    println("\n" ++ curGen.toString)
    /*pop.foldLeft(pop.head)( (t1, t2) => { println(Tree.toString(t1))
                                        println(Tree.findAverageFitness(t1, data))
                                        t2})*/
    if (curGen <= maxGen) {
      if (curGen % 10 == 0) {
        /*val last = pop.foldLeft(pop.head)( (t1, t2) => {
          if (t1 != t2) {
            println(Tree.toString(t1))
            println(Tree.findAverageFitness(t1, data))
          }//if
          t2})
        println(Tree.toString(last))
        println(Tree.findAverageFitness(last, data))*/
        val bb = findBest(pop, data)
        println("\nBest:\n" ++ Tree.toString(bb))
        println(Tree.findAverageFitness(bb, data).toString ++ "\n")
      }
      val nextGen: ListBuffer[Tree] = Tree.nextGeneration(pop, tourSize, data, ran)
      runAux(nextGen, data, tourSize, maxGen, curGen+1, ran)
    }
    else {
      findBest(pop, data)
    }
  }

  def run(pop: ListBuffer[Tree], maxDepth: Int, data: List[HashMap[String, Double]], tourSize: Int, maxGen: Int, ran: Random): Tree = {
    runAux(pop, data, tourSize, maxGen, 0, ran)
  }

  def main(args:Array[String]):Unit={

    val data = importData(2016, 2016).filter( inst => inst.nonEmpty )
    val pop = Tree.initializePopulation(populationSize, maxDepth, ran)
    val popRDD = sc.parallelize(pop.sliding(pop.size/4).toSeq)
    val runRDD = popRDD.map( pop => run(pop, maxDepth, data, tournamentSize, maxGenerations, ran))
    val result = runRDD.reduce({ (t1, t2) =>
      val f1 = Tree.findAverageFitness(t1, data)
      val f2 = Tree.findAverageFitness(t2, data)
      if (f1 < f2 && !f1.isNaN) t1
      else t2
    })
    println(Tree.toString(result))
    println(Tree.findAverageFitness(result, data))
  }
}
