
import scala.util.Random

/**
  * Created by Brent on 11/14/2016.
  */
object Tree {

  abstract class Tree
  case class Node(left: Tree, right: Tree, op: (Double, Double) => Double) extends Tree
  case class Leaf(x: Double) extends Tree

  val ran = new Random(System.currentTimeMillis)

  //val tree1 = Node(Node(Leaf(1), Leaf(2), "+"), Node(Leaf(3), Leaf(4), "-"), "*")

  def tree(left: Tree, right: Tree, op: (Double, Double) => Double) : Tree = {
    Node(left, right, op)

  }

  val add = (a : Double, b : Double) => a + b

  val sub = (a : Double, b : Double) => a - b

  val multiply = (a : Double, b : Double) => a * b

  val divide = (a : Double, b : Double) => a / b

  val pow = (a : Double, b : Double) => Math.pow(a,b)

  def opToString(op : (Double, Double) => Double) :String = {
    if (op == add) "+"
    else if (op == sub) "-"
    else if (op == multiply) "*"
    else if (op == divide) "/"
    //else if (op == pow) "^"
    else "NULL"
  }

  def randomOp() : (Double, Double) => Double = {
    val p = ran.nextDouble()
    if (p < .25) add
    else if (p < .5) sub
    else if (p < .75) multiply
    else if (p <= 1) divide
    else pow
  }

  def ranInit(depth : Int, max : Int) : Tree = {
    if (depth < max) {
      val op = randomOp()
      tree(ranInit(depth+1, max), ranInit(depth+1, max), op)
    }
    else tree(ran.nextGaussian()*ran.nextInt())
  }

  def randomInitialized(max: Int) : Tree = {
    ranInit(0, ran.nextInt() % max)
  }

  def tree(x: Double): Tree = {
    Leaf(x)
  }

  def printFunction(t: Tree): String = t match {
    case Node(l, r, op) => printFunction(l) + opToString(op) + printFunction(r)
    case Leaf(x) => x.toString
  }

  def evaluateFunction(t: Tree): Double = t match {
    case Node(l, r, op) => op (evaluateFunction(l), evaluateFunction(r))
    case Leaf(x) => x
  }

  def randomNode(t : Tree) : Tree = {
    val p = ran.nextDouble()
    t match {
      case Node(l, r, op) =>
        if (p <= .34) t
        else if (p <.66) randomNode(l)
        else randomNode(r)

      case Leaf(x) => t
    }
  }

  def combineTrees(p1: Tree, oldNode: Tree, newNode: Tree) : Tree = {
    //println("combTrees: p1 = " + printFunction(p1) + "\told = " + printFunction(oldNode) + "\tnew = " + printFunction(newNode))
    if (p1 == oldNode) {
      //println("if")
      newNode
    }
    else {
      p1 match {
        case Node(l, r, op) =>
          //println("Node")
          Node(combineTrees(l, oldNode, newNode), combineTrees(r, oldNode, newNode), op)
        case Leaf(x) => p1
        //println("Leaf")
      }
    }
  }

  def crossover(p1: Tree, p2: Tree) : Tree = {
    val node1 = randomNode(p1)
    //println("node1 = " + printFunction(node1))
    val node2 = randomNode(p2)
    //println("node2 = " + printFunction(node2))
    combineTrees(p1, node1, node2)
  }

  def main(args: Array[String]) {
    val t = randomInitialized(5)
    val t2 = randomInitialized(5)
    println("t = " + printFunction(t))
    println("t2 = " + printFunction(t2))
    println("c1 = " + printFunction(crossover(t, t2)))
    println("c2 = " + printFunction(crossover(t2, t)))

    println("t = " + evaluateFunction(t))
    println("t2 = " + evaluateFunction(t2))
    println("c1 = " + evaluateFunction(crossover(t, t2)))
    println("c2 = " + evaluateFunction(crossover(t2, t)))

    val tree1 = Node(Node(Leaf(1), Leaf(2), add), Node(Leaf(3), Leaf(4), sub), multiply)
    println (printFunction(tree1))
    println(evaluateFunction(tree1))
  }


}

