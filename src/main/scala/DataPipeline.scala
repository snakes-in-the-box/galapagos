/**
  * Created by Brent on 11/23/2016.
  */

import scala.collection.mutable
import scala.collection.mutable.HashMap

object DataPipeline {

  val dirPath = "C:/Users/Brent/Documents/School/DataPrac/FinalData/"

  def indexColumnsAux(s: Array[String], i: Int): HashMap[Int, String] = {
    if (i < s.size) {
      (new HashMap[Int, String] += (i -> s(i))) ++: indexColumnsAux(s, i + 1)
    }//if
    else new mutable.HashMap[Int, String]()
  }

  def indexColumns(s: String): HashMap[Int, String] = {
    val split = s.split(",")
    indexColumnsAux(split, 0)
  }

  def mapInstance(s: String, indexes: HashMap[Int, String]): HashMap[String, Double] = {
    val split = s.split(",")

  }

}
