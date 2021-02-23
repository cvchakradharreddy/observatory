package observatory

import observatory.Interaction.tileLocation
import observatory.Visualization.predictTemperature

class Grid(temperatures: Iterable[(Location, Temperature)]) extends Serializable {
  val gridTemp = new Array[Temperature](360 * 180)

  def gridLocationToIndex(grid: GridLocation) = {
    val x = grid.lon + 180
    val y = grid.lat + 89
    y * 360 + x
  }

  def preCompute() = {
    for {
      lat <- (90 until -90 by -1)
      lon <- -180 until 180
    } gridTemp(gridLocationToIndex(GridLocation(lat, lon))) = predictTemperature(temperatures, Location(lat, lon))
    this
  }

  def getTemp(gridLocation: GridLocation) = {
    gridTemp(gridLocationToIndex(gridLocation))
  }

  def merge(that: Grid) = {
    for (i <- 0 until gridTemp.length)
      this.gridTemp(i) += that.gridTemp(i)
    this
  }
}

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  def getGridComputed(temperatures: Iterable[(Location, Temperature)]) = new Grid(temperatures).preCompute()

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    getGridComputed(temperatures).getTemp
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val combinedGrid = temperaturess.par
      .map(getGridComputed(_))
      .reduce((grid1, grid2) => grid1.merge(grid2))
    (gridLoc: GridLocation) => combinedGrid.getTemp(gridLoc) / temperaturess.size
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = makeGrid(temperatures)
    (gridLoc: GridLocation) =>
      grid(gridLoc) - normals(gridLoc)
  }

}

