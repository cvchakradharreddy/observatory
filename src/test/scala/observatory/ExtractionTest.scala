package observatory

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object
  @Test def `should be able to extract data`: Unit = {
    val extractionRes = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    //extractionRes.take(10).foreach(println)
    assert(!extractionRes.isEmpty, "Extraction result shouldn't be empty")

  }

  @Test def `should be able to extract and compute avg temperature per location`: Unit = {
    val extractionRes = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    val avgTempPerLocation = Extraction.locationYearlyAverageRecords(extractionRes)
    //avgTempPerLocation.take(10).foreach(println)
    assert(!avgTempPerLocation.isEmpty, "Avg temperature per location shouldn't be empty")
  }


}
