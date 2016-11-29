/**
  * Created by Brent on 11/23/2016.
  */

import breeze.numerics.{atan, pow}

import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.Try

object DataPipeline {

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
    val split = s.split(",")
    hashInstanceAux(split, 0, indexes)
  }

  def calculateWetBulb(temp: Double, humidity: Double): Double = {
    temp * atan(0.151977 * pow(humidity + 8.313659, 1/2)) + atan(temp + humidity) - atan(humidity - 1.676331) + 0.00391838 * pow(humidity, 3/2) * atan(0.023101 * humidity) - 4.686035
  }

  def addGroundTruth(inst: HashMap[String, Double]): HashMap[String, Double] = {
    inst += ("wet-bulb" -> calculateWetBulb(inst("temp_air_2m_C"), inst("rh_2m_pct")))
  }

  def readFile(filePath: String): List[HashMap[String, Double]] = {
    val src = Source.fromFile(filePath)
    val fileList = src.getLines().toList
    val indexes = indexColumns(fileList.head)
    val instances = fileList.drop(1)
    instances.map( s => addGroundTruth(hashInstance(s, indexes)))
  }

}
