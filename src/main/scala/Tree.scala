
import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.util.Random



/**
  * Created by Brent on 11/14/2016.
  */
object Tree {

  abstract class Tree
  case class BinaryNode(left: Tree, right: Tree, op: (Double, Double) => Double) extends Tree
  case class UnaryNode(child: Tree, op: (Double) => Double) extends Tree
  case class Leaf(x: Double, feature: String) extends Tree

  val add = (a : Double, b : Double) => a + b

  val sub = (a : Double, b : Double) => a - b

  val multiply = (a : Double, b : Double) => a * b

  val divide = (a : Double, b : Double) => a / b

  val pow = (a : Double, b : Double) => Math.pow(a,b)

  val sin = (x: Double) => Math.sin(x)

  val cos = (x: Double) => Math.cos(x)

  val tan = (x: Double) => Math.tan(x)

  val arcsin = (x: Double) => Math.asin(x)

  val arccos = (x: Double) => Math.acos(x)

  val arctan = (x: Double) => Math.atan(x)

  val e = (x: Double) => x * Math.E

  val ln = (x: Double) => Math.log(x)

  val lg = (x: Double) => Math.log(x) / Math.log(2.0)

  val log = (x: Double) => Math.log10(x)

  def binOpToString(op : (Double, Double) => Double): String = {
    if (op == add) "+"
    else if (op == sub) "-"
    else if (op == multiply) "*"
    else if (op == divide) "/"
    else "NULL"
  }

  def binRandomOp(ran: Random) : (Double, Double) => Double = {
    val p = ran.nextDouble()
    if (p < .25) add
    else if (p < .5) sub
    else if (p < .75) multiply
    else divide
  }

  def unOpToString(op : (Double) => Double): String = {
    if (op == sin) "sin"
    else if (op == cos) "cos"
    else if (op == tan) "tan"
    else if (op == arcsin) "arcsin"
    else if (op == arccos) "arccos"
    else if (op == arctan) "arctan"
    else if (op == e) "e"
    else if (op == ln) "ln"
    else if (op == lg) "lg"
    else if (op == log) "log"
    else "NULL"
  }

  def unRandomOp(ran: Random) : (Double) => Double = {
    val p = ran.nextInt(9)
    if (p == 0) sin
    else if (p == 1) cos
    else if (p == 2) tan
    else if (p == 3) arcsin
    else if (p == 4) arccos
    else if (p == 5) arctan
    else if (p == 6) e
    else if (p ==6) ln
    else if (p == 7) lg
    else log
  }

  def randomFeature(ran: Random) : String = {
    val features = List("temp_soil_10cm_C","temp_air_60cm_C","temp_air_2m_C","temp_air_10m_C","rh_2m_pct","temp_dp_2m_C","rain_2m_inches","wind_speed_10m_mph","wind_speed_max_10m_mph","wind_direction_10m_deg","rfd_2m_wm2")
    features(ran.nextInt(features.length))
  }

  def randomInitializedAux(depth : Int, max : Int, ran: Random) : Tree = {
    if (depth < max) {
      if (ran.nextDouble() <= .5) {
        val op = binRandomOp(ran)
        BinaryNode(randomInitializedAux(depth + 1, max, ran), randomInitializedAux(depth + 1, max, ran), op)
      }//if
      else UnaryNode(randomInitializedAux(depth + 1, max, ran), unRandomOp(ran))
    }
    else Leaf(ran.nextGaussian(), randomFeature(ran))
  }

  def randomInitialized(max: Int, ran: Random) : Tree = {
    randomInitializedAux(0, max, ran)
  }

  def toString(t: Tree): String = t match {
    case BinaryNode(l, r, op) => "(" ++ toString(l) ++ " " ++ binOpToString(op) ++ " " ++ toString(r) ++ ")"
    case UnaryNode(c, op) => "(" ++ unOpToString(op) ++ "( " ++ toString(c) ++ " )" ++ ")"
    case Leaf(x, f) => x.toString ++ ":" ++ f
  }

  def calculateWetBulb(t: Tree, inst: HashMap[String, Double]): Double = t match {
    case BinaryNode(l, r, op) => op (calculateWetBulb(l, inst), calculateWetBulb(r, inst))
    case UnaryNode(c, op) => op (calculateWetBulb(c, inst))
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
      case UnaryNode(c, op) => (new ListBuffer[Tree]() += t) ++: vectorize(c)
      case BinaryNode(l, r, op) => (new ListBuffer[Tree]() += t) ++: vectorize(l) ++: vectorize(r)
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
        case BinaryNode(l, r, op) =>
          BinaryNode(combineTrees(l, oldNode, newNode), combineTrees(r, oldNode, newNode), op)
        case UnaryNode(c, op) => UnaryNode(combineTrees(c, oldNode, newNode), op)
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
      case BinaryNode(l, r, op) => (new ListBuffer[Tree]() ++: vectorizeLeaves(l)) ++: vectorizeLeaves(r)
      case UnaryNode(c, op) => new ListBuffer[Tree]() ++: vectorizeLeaves(t)
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
        if (f1 < f2 || f2 == Double.NaN) t1
        else t2
      }
  }

  def deathSelection(pop: ListBuffer[Tree], tourSize: Int, insts: List[HashMap[String, Double]], ran: Random) : Tree = {
    val chosen = randomTrees(pop, tourSize, ran)
    chosen.foldLeft(chosen.head) {
      (t1: Tree, t2: Tree) =>
        val f1 = findAverageFitness(t1, insts)
        val f2 = findAverageFitness(t2, insts)
        if (f1 > f2 || f1 == Double.NaN) t1
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

