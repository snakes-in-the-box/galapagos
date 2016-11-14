/**
  * Created by brad on 11/14/16.
  */
object Tree {

  sealed abstract class BinaryTree

  case class Leaf(value: Int) extends BinaryTree

  case class Node(op: (Int, Int) => Int, leaves: BinaryTree*) extends BinaryTree

  def calc(tree: BinaryTree): Int = tree match {
    case Leaf(v) => v
    case Node(op, head: BinaryTree, leaves@_*) => leaves.foldLeft(calc(head))((a, b) => op(a, calc(b)))
  }

  object Operators {
    def +(a: Int, b: Int): Int = a + b

    def *(a: Int, b: Int): Int = a * b

    def -(a: Int, b: Int): Int = a - b

    def /(a: Int, b: Int): Int = a / b

  }

  val tree = Node(Operators.+, Node(Operators.*, Leaf(9), Leaf(3)), Leaf(1))

  def main(args: Array[String]): Unit = {
    println(calc(tree))
  }

}
