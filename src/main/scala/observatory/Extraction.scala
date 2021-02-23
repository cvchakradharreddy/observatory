package observatory


import java.time.LocalDate
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  case class Stations(stnid: String, wbanid: String, lat: Double, lon: Double)

  case class Temp(stnid: String, wbanid: String, month: Int, day: Int, temperature: Temperature)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationsData = sc.parallelize(Source.fromInputStream(getClass.getResourceAsStream(stationsFile)).getLines().toSeq, 10)
    val tempData = sc.parallelize(Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile)).getLines().toSeq, 10)

    import spark.implicits._
    val stationDS = stationsData
      .map(_.split(",", -1))
      .filter(col => !(col(2).isEmpty || col(3).isEmpty))
      .map(col => Stations(col(0), col(1), col(2).toDouble, col(3).toDouble))
      .toDS()
    val tempDS = tempData
      .map(_.split(",", -1))
      .map(col => Temp(col(0), col(1), col(2).toInt, col(3).toInt, col(4).toDouble))
      .toDS()

    def ftoc(f: Temperature) = (f - 32) * 5 / 9

    stationDS
      .joinWith(tempDS, (stationDS("stnid") === tempDS("stnid")) && (stationDS("wbanid") === tempDS("wbanid")))
      .rdd
      .map { case (station, temp) =>
        (LocalDate.of(year, temp.month, temp.day), Location(station.lat, station.lon), ftoc(temp.temperature))
      }.collect()


  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    import spark.implicits._
    import org.apache.spark.sql.expressions.scalalang.typed
    val recordsDS = sc.parallelize(records.toSeq, 100).map(rec => (rec._2, rec._3)).toDS()
    recordsDS
      .groupByKey(_._1)
      .agg(typed.avg[(Location, Temperature)](_._2))
      .collect()
  }

}
