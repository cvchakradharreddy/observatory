package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.annotation.tailrec

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    import math._
    val r = 6371.0
    val p = 6

    def d(x: Location, xi: Location): Double = {
      val delta =
        if (x == xi) 0
        else if ((x.lat + xi.lat == 0) && (x.lon + xi.lon == 0)) Pi
        else {
          val xLat = x.lat.toRadians
          val xLon = x.lon.toRadians
          val xiLat = xi.lat.toRadians
          val xiLon = xi.lon.toRadians
          acos(sin(xLat) * sin(xiLat) + cos(xLat) * cos(xiLat) * cos(abs(xLon - xiLon)))
        }
      r * delta
    }

    /*    def w(d: Double) = 1 / math.pow(d, p)

        val tempPar = temperatures
          .par
          .map(r => (d(location, r._1), r))

        val closest = tempPar.filter(_._1 < 1)
        if (closest.isEmpty) {
          val result = tempPar
            .map(r => {
              val weight = w(r._1)
              (weight * r._2._2, weight)
            })
            .reduce((vw1, vw2) => (vw1._1 + vw2._1, vw1._2 + vw2._2))
          result._1 / result._2
        } else {
          closest.reduce((r1, r2) => if (r1._1 < r2._1) r1 else r2)._2._2
        }*/
    @tailrec
    def loop(tempIt: Iterator[(Location, Temperature)], accVal: Double, accWeight: Double): Double = {
      if (tempIt.hasNext) {
        val (loc, temp) = tempIt.next()
        val distance = d(location, loc)
        if (distance < 1) temp
        else {
          val weight = 1 / math.pow(distance, p)
          loop(tempIt, accVal + weight * temp, accWeight + weight)
        }
      } else {
        accVal / accWeight
      }
    }

    loop(temperatures.toIterator, 0.0, 0.0)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val found = points.filter(_._1 == value)
    if (found.isEmpty) {
      val sortedPoints = points.toSeq.sortBy(_._1)
      if (value <= sortedPoints.head._1) sortedPoints.head._2
      else if (value >= sortedPoints.last._1) sortedPoints.last._2
      else {
        val interval = sortedPoints.zip(sortedPoints.tail).filter(p => value > p._1._1 && value < p._2._1).head

        def f(x: Double, x0: Double, y0: Int, x1: Double, y1: Int) = {
          val w = (x - x0) / (x1 - x0)
          math.round(y0 * (1 - w) + y1 * w).toInt
        }

        Color(f(value, interval._1._1, interval._1._2.red, interval._2._1, interval._2._2.red),
          f(value, interval._1._1, interval._1._2.green, interval._2._1, interval._2._2.green),
          f(value, interval._1._1, interval._1._2.blue, interval._2._1, interval._2._2.blue))
      }
    } else found.head._2
  }


  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    import com.sksamuel.scrimage.Pixel

    val width = 360
    val height = 180
    val topLeftLocation = (90.0, -180.0)

    def pixelToLocation(pixel: (Long, Long)) = Location(topLeftLocation._1 - pixel._1, topLeftLocation._2 + pixel._2)

    def indexToPixel(i: Long) = (i / width, i % width)

    def indexToLocation(i: Long) = pixelToLocation(indexToPixel(i))

    val pixels = sc.range(0, width * height, numSlices = 10)
      .map(i => {
        val color = interpolateColor(colors, predictTemperature(temperatures, indexToLocation(i)))
        Pixel(color.red, color.green, color.blue, 255)
      })
      .collect()
    Image(width, height, pixels)
  }

}

