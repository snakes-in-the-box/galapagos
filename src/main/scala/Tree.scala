
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.util.Random
import DataPipeline._

/**
  * Created by Brent on 11/14/2016.
  */
object Tree {

  abstract class Tree
  case class Node(left: Tree, right: Tree, op: (Double, Double) => Double) extends Tree
  case class Leaf(x: Double, feature: String) extends Tree

  def tree(left: Tree, right: Tree, op: (Double, Double) => Double) : Tree = {
    Node(left, right, op)
  }

  def tree(x: Double, feature: String): Tree = {
    Leaf(x, feature: String)
  }

  val add = (a : Double, b : Double) => a + b

  val sub = (a : Double, b : Double) => a - b

  val multiply = (a : Double, b : Double) => a * b

  val divide = (a : Double, b : Double) => a / b

  val pow = (a : Double, b : Double) => Math.pow(a,b)

  def opToString(op : (Double, Double) => Double): String = {
    if (op == add) "+"
    else if (op == sub) "-"
    else if (op == multiply) "*"
    else if (op == divide) "/"
    //else if (op == pow) "^"
    else "NULL"
  }

  def randomOp(ran: Random) : (Double, Double) => Double = {
    val p = ran.nextDouble()
    if (p < .25) add
    else if (p < .5) sub
    else if (p < .75) multiply
    else if (p <= 1) divide
    else pow
  }

  def randomFeature(ran: Random) : String = {
    val features = List("avg_temp_soil_10cm_C","min_temp_soil_10cm_C","max_temp_soil_10cm_C","avg_temp_air_60cm_C","min_temp_air_60cm_C","max_temp_air_60cm_C","avg_temp_air_2m_C","min_temp_air_2m_C","max_temp_air_2m_C","avg_temp_air_10m_C","min_temp_air_10m_C","max_temp_air_10m_C","avg_rh_2m_pct","avg_temp_dp_2m_C","min_temp_dp_2m_C","max_temp_dp_2m_C","sum_rain_2m_inches","avg_wind_speed_10m_mph","wind_speed_max_10m_mph","wind_direction_10m_deg","avg_rfd_2m_wm2","trf_2m_MJm2")
    features(ran.nextInt(features.length))
  }

  def randomInitializedAux(depth : Int, max : Int, ran: Random) : Tree = {
    if (depth < max) {
      val op = randomOp(ran)
      tree(randomInitializedAux(depth+1, max, ran), randomInitializedAux(depth+1, max, ran), op)
    }
    else tree(ran.nextGaussian(), randomFeature(ran))
  }

  def randomInitialized(max: Int, ran: Random) : Tree = {
    randomInitializedAux(0, max, ran)
  }

  def printFunction(t: Tree): String = t match {
    case Node(l, r, op) => printFunction(l) + opToString(op) + printFunction(r)
    case Leaf(x, f) => x.toString + ":" + f.toString
  }


  def dataValue(feature: String, inst: HashMap[String, Double]) : Double = {
    inst(feature)
  }

  def findInstanceFitness(t: Tree, inst: HashMap[String, Double]): Double = t match {
    case Node(l, r, op) => op (findInstanceFitness(l, inst), findInstanceFitness(r, inst))
    case Leaf(x, f) => x * dataValue(f, inst)
  }

  def sumAverages(t: Tree, insts: List[HashMap[String, Double]]) : Double = {
    if (insts != Nil) {
      findInstanceFitness(t, insts.head) + sumAverages(t, insts)
    }//if
    else 0
  }

  def findAverageFitness(t: Tree, insts: List[HashMap[String, Double]]): Double = {
    val sum = sumAverages(t, insts)
    sum / insts.size:Double
  }

  def vectorize(t: Tree): ListBuffer[Tree] = {
    t match {
      case Node(l, r, op) => (new ListBuffer[Tree]() += t) ++: vectorize(l) ++: vectorize(r)
      case Leaf(x, f) => new ListBuffer[Tree]() += t
    }
  }

  def randomNode(t : Tree, ran: Random) : Tree = {
    val nodes = vectorize(t)
    nodes(ran.nextInt(nodes.size))
  }

  def combineTrees(t: Tree, oldNode: Tree, newNode: Tree) : Tree = {
    if (t == oldNode) {
      newNode
    }
    else {
      t match {
        case Node(l, r, op) =>
          Node(combineTrees(l, oldNode, newNode), combineTrees(r, oldNode, newNode), op)
        case Leaf(x, f) => t
      }
    }
  }

  def crossover(p1: Tree, p2: Tree, ran: Random): Tree = {
    val node1 = randomNode(p1, ran)
    val node2 = randomNode(p2, ran)
    combineTrees(p1, node1, node2)
  }

  def vectorizeLeaves(t: Tree): ListBuffer[Tree] = {
    t match {
      case Node(l, r, op) => new ListBuffer[Tree]() ++: vectorize(l) ++: vectorize(r)
      case Leaf(x, f) => new ListBuffer[Tree]() += t
    }
  }

  def randomLeaf(t: Tree, ran: Random): Tree = {
    val leaves = vectorizeLeaves(t)
    leaves(ran.nextInt(leaves.size))
  }

  def mutateCoefficient(t: Tree, ran: Random): Tree = {
    val Leaf (x, feature) = randomLeaf(t, ran)
    combineTrees(t, Leaf (x, feature), Leaf(x + ran.nextGaussian(), feature))
  }

  def mutate(t: Tree, ran: Random) : Tree = {
    mutateCoefficient(t, ran)
  }

  def randomTrees(pop: ListBuffer[Tree], size: Int, ran: Random): ListBuffer[Tree] = {
    if (size > 0) (new ListBuffer[Tree] += pop(ran.nextInt(pop.size))) ++: randomTrees(pop, size-1, ran)
    else new ListBuffer[Tree]()
  }

  def loveSelection(pop: ListBuffer[Tree], tourSize: Int, insts: List[HashMap[String, Double]], ran: Random) : Tree = {
      val chosen = randomTrees(pop, tourSize, ran)
      chosen.foldLeft(chosen.head) {
      (t1: Tree, t2: Tree) =>
        if (findAverageFitness(t1, insts) > findAverageFitness(t2, insts)) t1
        else t2
      }
  }

  def deathSelection(pop: ListBuffer[Tree], tourSize: Int, insts: List[HashMap[String, Double]], ran: Random) : Tree = {
    val chosen = randomTrees(pop, tourSize, ran)
    chosen.foldLeft(chosen.head) {
      (t1: Tree, t2: Tree) =>
        if (findAverageFitness(t1, insts) < findAverageFitness(t2, insts)) t1
        else t2
    }
  }

  def intercourse(pop: ListBuffer[Tree], tourSize: Int, insts: List[HashMap[String, Double]], ran: Random): Tree = {
    val t1 = loveSelection(pop, tourSize, insts, ran)
    val t2 = loveSelection(pop, tourSize, insts, ran)
    crossover(t1, t2, ran)
  }

  def initializePopulation(size: Int, maxDepth: Int, ran: Random): ListBuffer[Tree] = {
    if (size > 0) {
      (new ListBuffer[Tree]() += randomInitialized(ran.nextInt(maxDepth)+1, ran)) ++: initializePopulation(size-1, maxDepth, ran)
    }
    else new ListBuffer[Tree]()
  }

  def replaceIndividual(pop: ListBuffer[Tree], oldTree: Tree, newTree: Tree): ListBuffer[Tree] = {
    pop.map({ t =>
      if (t == oldTree) newTree
      else t
    })
  }

  def main(args: Array[String]) {
    val ran = new Random(System.currentTimeMillis)
    val pop = initializePopulation(5, 5, ran)
    val data = DataPipeline.readFile("2012_hourly/2012-1.csv")

  }


}

