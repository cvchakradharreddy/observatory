package observatory

import observatory.Manipulation.makeGrid

object Main extends App {

  val yearlyData = sc
    .parallelize(fromYear to toYear)
    .map(year =>
      (year, Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv"))))

  val grids = yearlyData
    .map { case (year, yearAvgData) => (year, makeGrid(yearAvgData)) }
    .cache()


  spark.stop()
}
