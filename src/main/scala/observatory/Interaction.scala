package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}

import scala.collection.parallel.CollectionsHaveToParArray

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    import scala.math._
    Location(toDegrees(atan(sinh(Pi * (1.0 - 2.0 * tile.y.toDouble / (1 << tile.zoom))))),
      tile.x.toDouble / (1 << tile.zoom) * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val width = 256
    val height = 256

    val pixels = sc.parallelize(genSubtiles(tile, 8).sortBy(tile => (tile.y, tile.x)))
      .map(tile => {
        val color = interpolateColor(colors, predictTemperature(temperatures, tileLocation(tile)))
        Pixel(color.red, color.green, color.blue, 127)
      }).collect()
    Image(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {

    yearlyData.par.foreach {
      case (year, data) =>
        for {
          zoom <- (0 to 3).par
          subTile <- genSubtiles(Tile(0, 0, 0), zoom)
        } generateImage(year, subTile, data)
    }
  }

  def genSubtiles(fromTile: Tile, toZoom: Int) = (fromTile.zoom + 1 to fromTile.zoom + toZoom).foldLeft(Array(fromTile))((tiles, zoom) => tiles.flatMap(tile =>
    Array(Tile(2 * tile.x, 2 * tile.y, zoom),
      Tile(2 * tile.x + 1, 2 * tile.y, zoom),
      Tile(2 * tile.x, 2 * tile.y + 1, zoom),
      Tile(2 * tile.x + 1, 2 * tile.y + 1, zoom))))

}
