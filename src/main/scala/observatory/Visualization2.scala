package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.{genSubtiles, tileLocation}
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             point: CellPoint,
                             d00: Temperature,
                             d01: Temperature,
                             d10: Temperature,
                             d11: Temperature
                           ): Temperature = {
    val (x, y) = (point.x, point.y)
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: GridLocation => Temperature,
                     colors: Iterable[(Temperature, Color)],
                     tile: Tile
                   ): Image = {
    val width = 256
    val height = 256

    val pixels = genSubtiles(tile, 8).sortBy(tile => (tile.y, tile.x))
      .map(tile => {
        val location = tileLocation(tile)
        val gridLat = location.lat.toInt
        val gridLon = location.lon.toInt
        val d00 = grid(GridLocation(gridLat, gridLon))
        val d01 = grid(GridLocation(gridLat + 1, gridLon))
        val d10 = grid(GridLocation(gridLat, gridLon + 1))
        val d11 = grid(GridLocation(gridLat + 1, gridLon + 1))
        val point = CellPoint(location.lon - gridLon, location.lat - gridLat)

        val color = interpolateColor(colors, bilinearInterpolation(point, d00, d01, d10, d11))
        Pixel(color.red, color.green, color.blue, 127)
      })
    Image(width, height, pixels)
  }

}
