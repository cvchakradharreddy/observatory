import org.apache.spark.SparkContext
import org.apache.spark.sql.SparkSession
import org.apache.log4j.{Level, Logger}

package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1
  val spark: SparkSession = SparkSession
    .builder()
    .appName("Observatory")
    .master("local[*]")
    .getOrCreate()
  val sc: SparkContext = spark.sparkContext

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val tempColors: List[(Double, Color)] = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

  val devColors: List[(Double, Color)] = List(
    (7.0, Color(0, 0, 0)),
    (4.0, Color(255, 0, 0)),
    (2.0, Color(255, 255, 0)),
    (0.0, Color(255, 255, 255)),
    (-2.0, Color(0, 255, 255)),
    (-7.0, Color(0, 0, 255))
  )

  val fromYear = 1975
  val toYear = 2015
  val devYear = 1991

}
