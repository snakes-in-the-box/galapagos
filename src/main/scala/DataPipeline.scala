/**
  * Created by Brent on 11/23/2016.
  */

//import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.Try

object DataPipeline {

  val dirPath = "C:/Users/Brent/Documents/School/DataPrac/FinalData/"

  def indexColumnsAux(s: Array[String], i: Int): HashMap[Int, String] = {
    if (i < s.length) {
      (new HashMap[Int, String] += (i -> s(i))) ++: indexColumnsAux(s, i + 1)
    }//if
    else new HashMap[Int, String]()
  }

  def indexColumns(s: String): HashMap[Int, String] = {
    val split = s.split(",")
    indexColumnsAux(split, 0)
  }

  def hashInstanceAux(s: Array[String], i: Int, indexes: HashMap[Int, String]): HashMap[String, Double] = {
    if (i < s.length && Try(s(i).toDouble).isSuccess) {
      (new HashMap[String, Double]() += (indexes(i) -> s(i).toDouble)) ++: hashInstanceAux(s, i+1, indexes)
    }
    else if (i < s.length) hashInstanceAux(s, i+1, indexes)
    else new HashMap[String, Double]()
  }

  def hashInstance(s: String, indexes: HashMap[Int, String]): HashMap[String, Double] = {
    //println("hash!")
    val split = s.split(",")
    //for (s <- split) println(s)
    hashInstanceAux(split, 0, indexes)
  }

  def readFile(fName: String): List[HashMap[String, Double]] = {
    //println("read!")
    val src = Source.fromFile(dirPath + fName)
    val fileList = src.getLines().toList
    val indexes = indexColumns(fileList.head)
    //println(indexes.toString())
    val instances = fileList.drop(1)
    instances.map( s => hashInstance(s, indexes))
    //for (s <- instances) println(hashInstance(s, indexes).toString())
  }

  def main(args: Array[String]): Unit = {
    val data = readFile("2012_hourly/2012-1.csv")
    for (inst <- data) println(inst.toString())
  }

}
