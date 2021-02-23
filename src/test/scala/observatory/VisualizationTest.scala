package observatory

import org.junit.Assert._
import org.junit.Test

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  // Implement tests for the methods of the `Visualization` object
  @Test def `Temperature prediction d > 1km` = {
    val loc = Location(45.0, -180.0)
    val temperatures = List(
      (Location(45.0, -178.9), 10.0),
      (Location(45.0, -179), 20.0)
    )
    val t = Visualization.predictTemperature(temperatures, loc)
    assert(math.abs(t - 20) < math.abs(t - 10), s"Temperature $t should be more closer to 20")
  }

  @Test def `Temperature prediction d < 1km` = {
    val loc = Location(45.0, -180.0)
    val temperatures = List(
      (Location(45.0, -179.99), 10.0),
      (Location(45.0, -178.9), 20.0)
    )
    val t = Visualization.predictTemperature(temperatures, loc)
    assert(math.abs(t - 10) < math.abs(t - 20), s"Temperature $t should be close to 10")
  }

  @Test def `Temperature prediction one with d = 0 and another is antipode` = {
    val loc = Location(45.0, -180.0)
    val temperatures = List(
      (Location(-45.0, 180.0), 10.0),
      (Location(45.0, -180.0), 20.0)
    )
    val t = Visualization.predictTemperature(temperatures, loc)
    assert(t == 20.0, s"Temperature $t should be equal to 20")
  }

  def interpolate(temperature: Temperature) = {
    val tempColorList = List(
      (60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0))
    )
    Visualization.interpolateColor(tempColorList, temperature)
  }

  @Test def `Interpolate color for outside boundary` = {
    val c1 = interpolate(70.0)
    val c2 = interpolate(-70.0)
    assertEquals(Color(255, 255, 255), c1)
    assertEquals(Color(0, 0, 0), c2)
  }

  @Test def `Interpolate color for inside boundary: Existing` = {
    val c1 = interpolate(12.0)
    val c2 = interpolate(-27.0)
    assertEquals(Color(255, 255, 0), c1)
    assertEquals(Color(255, 0, 255), c2)
  }

  @Test def `Interpolate color for inside boundary` = {
    val c1 = interpolate(34.0)
    val c2 = interpolate(-49.0)
    assertEquals(Color(255,18,18), c1)
    assertEquals(Color(43,0,113), c2)
  }

}
