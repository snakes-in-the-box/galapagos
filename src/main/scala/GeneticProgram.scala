
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by Brent on 11/14/2016.
  */
object Tree {

  abstract class Tree
  case class Node(left: Tree, right: Tree, op: (Double, Double) => Double) extends Tree
  case class Leaf(x: Double, feature: String) extends Tree

  //val ran = new Random(System.currentTimeMillis)

  //val tree1 = Node(Node(Leaf(1), Leaf(2), "+"), Node(Leaf(3), Leaf(4), "-"), "*")

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
    val numCols = 2
    val p = ran.nextInt(numCols)// % numCols
    p match {
      case 0 => "avg_temp_soil_10cm_C"
      case 1 => "avg_temp_air_60cm_C"
        //etc
    }
  }

  def ranInit(depth : Int, max : Int, ran: Random) : Tree = {
    if (depth < max) {
      val op = randomOp(ran)
      tree(ranInit(depth+1, max, ran), ranInit(depth+1, max, ran), op)
    }
    else tree(ran.nextGaussian()*ran.nextInt(), randomFeature(ran))
  }

  def randomInitialized(max: Int, ran: Random) : Tree = {
    ranInit(0, max, ran)
  }

  def printFunction(t: Tree): String = t match {
    case Node(l, r, op) => printFunction(l) + opToString(op) + printFunction(r)
    case Leaf(x, f) => x.toString + ":" + f.toString
  }


  def dataValue(feature: String, inst: List[Double]) : Double = {
    //Will return the value of some feature column of some specified instance
    1
  }

  def findInstanceFitness(t: Tree, inst: List[Double]): Double = t match {
    case Node(l, r, op) => op (findInstanceFitness(l, inst), findInstanceFitness(r, inst))
    case Leaf(x, f) => x * dataValue(f, inst)
  }

  def sumAverages(t: Tree, insts: List[List[Double]]) : Double = {
    if (insts != Nil) {
      findInstanceFitness(t, insts(0)) + sumAverages(t, insts)
    }//if
    else 0
  }

  def findAverageFitness(t: Tree, insts: List[List[Double]]): Double = {
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
    //println("combTrees: t = " + printFunction(t) + "\told = " + printFunction(oldNode) + "\tnew = " + printFunction(newNode))
    if (t == oldNode) {
      //println("if")
      newNode
    }
    else {
      t match {
        case Node(l, r, op) =>
          //println("Node")
          Node(combineTrees(l, oldNode, newNode), combineTrees(r, oldNode, newNode), op)
        case Leaf(x, f) => t
        //println("Leaf")
      }
    }
  }

  def crossover(p1: Tree, p2: Tree, ran: Random) : Tree = {
    val node1 = randomNode(p1, ran)
    //println("node1 = " + printFunction(node1))
    val node2 = randomNode(p2, ran)
    //println("node2 = " + printFunction(node2))
    combineTrees(p1, node1, node2)
  }

  def randomTrees(pop: ListBuffer[Tree], size: Int, ran: Random): ListBuffer[Tree] = {
    val chosen : ListBuffer[Tree] = new ListBuffer[Tree]
    for (i <- 0 until size) {
      chosen += pop(ran.nextInt(pop.size))
    }//for
    chosen
  }

  def loveSelection(pop: ListBuffer[Tree], tourSize: Int, insts: List[List[Double]], ran: Random) : Tree = {
      val chosen = randomTrees(pop, tourSize, ran)
      chosen.foldLeft(chosen(0)) {
      (t1: Tree, t2: Tree) =>
        if (findAverageFitness(t1, insts) > findAverageFitness(t2, insts)) t1
        else t2
      }
  }

  def deathSelection(pop: ListBuffer[Tree], tourSize: Int, insts: List[List[Double]], ran: Random) : Tree = {
    val chosen = randomTrees(pop, tourSize, ran)
    chosen.foldLeft(chosen(0)) {
      (t1: Tree, t2: Tree) =>
        if (findAverageFitness(t1, insts) < findAverageFitness(t2, insts)) t1
        else t2
    }
  }

  def initializePopulation(size: Int, maxDepth: Int, ran: Random): ListBuffer[Tree] = {
    if (size > 0) {
      (new ListBuffer[Tree]() += randomInitialized(ran.nextInt(maxDepth)+1, ran)) ++: initializePopulation(size-1, maxDepth, ran)
    }
    else new ListBuffer[Tree]()
  }

  def main(args: Array[String]) {
    val ran = new Random(System.currentTimeMillis)
    val pop = initializePopulation(5, 5, ran)
    println(pop.size)
    println(pop.toString())
    /*
    val t = randomInitialized(5, ran)
    val t2 = randomInitialized(5, ran)
    println("t = " + printFunction(t))
    println("t2 = " + printFunction(t2))
    println("c1 = " + printFunction(crossover(t, t2, ran)))
    println("c2 = " + printFunction(crossover(t2, t, ran)))
    */

    //val tree1 = Node(Node(Leaf(1), Leaf(2), add), Node(Leaf(3), Leaf(4), sub), multiply)
    //println (printFunction(tree1))
    //println(evaluateFunction(tree1))
  }


}

