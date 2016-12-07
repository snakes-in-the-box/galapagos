
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.util.Random


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
    Leaf(x, feature)
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
    else "NULL"
  }

  def randomOp(ran: Random) : (Double, Double) => Double = {
    val p = ran.nextDouble()
    if (p < .25) add
    else if (p < .5) sub
    else if (p < .75) multiply
    else divide
  }

  def randomFeature(ran: Random) : String = {
    val features = List("temp_soil_10cm_C","temp_air_60cm_C","temp_air_2m_C","temp_air_10m_C","rh_2m_pct","temp_dp_2m_C","rain_2m_inches","wind_speed_10m_mph","wind_direction_10m_deg","rfd_2m_wm2")
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

  def toString(t: Tree): String = t match {
    case Node(l, r, op) => "(" ++ toString(l) ++ " " ++ opToString(op) ++ " " ++ toString(r) ++ ")"
    case Leaf(x, f) => x.toString ++ ":" ++ f
  }

  def calculateWetBulb(t: Tree, inst: HashMap[String, Double]): Double = t match {
    case Node(l, r, op) => op (calculateWetBulb(l, inst), calculateWetBulb(r, inst))
    case Leaf(x, f) =>  if (inst.contains(f)) x * inst(f)
                        else 0
  }

  def findInstanceFitness(t: Tree, inst: HashMap[String, Double]): Double = {
    if (inst.contains("wet-bulb")) Math.abs(inst("wet-bulb") - calculateWetBulb(t, inst))
    else 0
  }

  def sumAverages(t: Tree, insts: List[HashMap[String, Double]]) : Double = {
    insts.map(
      inst => findInstanceFitness(t, inst)
    ).sum
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
      case Node(l, r, op) => (new ListBuffer[Tree]() ++: vectorizeLeaves(l)) ++: vectorizeLeaves(r)
      case Leaf(x, f) => new ListBuffer[Tree]() += t
    }
  }

  def randomLeaf(t: Tree, ran: Random): Tree = {
    val leaves = vectorizeLeaves(t)
    leaves(ran.nextInt(leaves.size))
  }

  def mutateCoefficient(t: Tree, ran: Random): Tree = t match {
    case Leaf(x, f) => val l = randomLeaf(t, ran)
                      combineTrees(t, Leaf (x, f), Leaf(x + ran.nextGaussian(), f))
    case default => t
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
        val f1 = findAverageFitness(t1, insts)
        val f2 = findAverageFitness(t2, insts)
        if (f1 < f2 || f2.isNaN) t1
        else t2
      }
  }

  def deathSelection(pop: ListBuffer[Tree], tourSize: Int, insts: List[HashMap[String, Double]], ran: Random) : Tree = {
    val chosen = randomTrees(pop, tourSize, ran)
    chosen.foldLeft(chosen.head) {
      (t1: Tree, t2: Tree) =>
        val f1 = findAverageFitness(t1, insts)
        val f2 = findAverageFitness(t2, insts)
        if (f1 > f2 || f1.isNaN) t1
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

  def birthIndividual(pop: ListBuffer[Tree], tourSize: Int, insts: List[HashMap[String, Double]], ran: Random): Tree = {
    val baby = intercourse(pop, tourSize, insts, ran)
    if (ran.nextDouble() <= .05) mutate(baby, ran)
    else baby
  }
  
  def eugenics(pop: ListBuffer[Tree], tourSize: Int, insts: List[HashMap[String, Double]], newTree: Tree, ran: Random): ListBuffer[Tree] = {
    val weak = deathSelection(pop, tourSize, insts, ran)
    replaceIndividual(pop, weak, newTree)
  }

  def nextGeneration(pop: ListBuffer[Tree], tourSize: Int, insts: List[HashMap[String, Double]], ran: Random): ListBuffer[Tree] = {
    val newTree = birthIndividual(pop, tourSize, insts, ran)
    eugenics(pop, tourSize, insts, newTree, ran)
  }

}

