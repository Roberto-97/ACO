package Entities

import Entities.DistanceStrategies.DistanceStrategies

object Tsp {

  def nodeBranching(colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Double = {
    var numBranches = Vector.fill(tspParameters.numberCities)(0.0)
    (0 until tspParameters.numberCities).map((m) => {
      var min = colonie.pheremone(m)(tspParameters.nearestNeighborsMatrix(m)(1).get)
      var max = colonie.pheremone(m)(tspParameters.nearestNeighborsMatrix(m)(1).get)
      (1 until ep.nnAnts).map(i => {
        if (colonie.pheremone(m)(tspParameters.nearestNeighborsMatrix(m)(i).get) > max) {
          max = colonie.pheremone(m)(tspParameters.nearestNeighborsMatrix(m)(i).get)
        }
        if (colonie.pheremone(m)(tspParameters.nearestNeighborsMatrix(m)(i).get) < min) {
          min = colonie.pheremone(m)(tspParameters.nearestNeighborsMatrix(m)(i).get)
        }
      })
      val cutoff = min + tspParameters.lambda * (max - min)
      (0 until ep.nnAnts).map(i => {
        if (colonie.pheremone(m)(tspParameters.nearestNeighborsMatrix(m)(i).get) > cutoff)
          numBranches = numBranches.updated(m, numBranches(m) + 1.0)
      })
    })
    val avg = numBranches.reduce((x, y) => x + y)
    avg / (tspParameters.numberCities * 2)
  }

  def heuristic(i: Int, j: Int, distance: Vector[Vector[Int]]): Double = {
    1.0 / (distance(i)(j) + 0.1)
  }

  def computeNearestNeighborsMatrix(ep: ExecutionParameters, tspParameters: TspParameters): Unit = {
    var nn: Int = 0
    println("Computing nearest neighbor lists ..")
    nn = ep.nnLs.max(ep.nnAnts)
    if (nn >= tspParameters.numberCities)
      nn = tspParameters.numberCities - 1
    require(tspParameters.numberCities > nn, "Number of cities must be mayor than depth of nearest ")

    tspParameters.nearestNeighborsMatrix = Array.fill(tspParameters.numberCities)(Array.fill(nn)(Option(0)))
    var node = 0
    while (node < tspParameters.numberCities) {
      var auxVector = (0 until tspParameters.numberCities).map(i => (i, tspParameters.distance(node)(i))).toVector
      auxVector = auxVector.updated(node, (node, Int.MaxValue))
      val nearestCities = auxVector
        .sortBy(_._2)
        .map(t => Option(t._1))
      for (i <- 0 until nn) {
        tspParameters.nearestNeighborsMatrix(node)(i) = nearestCities(i)
      }
      node += 1
    }
    println("done ..")
  }

  def computeTourLength(tour: Vector[Option[Int]], numberCities: Int, distance: Vector[Vector[Int]]): Int = {
    (0 until numberCities)
      .map((city) => distance(tour(city).get)(tour(city + 1).get))
      .reduce((distancex, distancey) => distancex + distancey)
  }

  def computeDistances(numberCities: Int, distanceStrategy: DistanceStrategies, nodePtr: Vector[Point]): Vector[Vector[Int]] = {
    (0 until numberCities)
      .map((i) => (0 until numberCities)
        .map((j) => distanceStrategy.computeDistance(i, j, nodePtr)).toVector).toVector
  }

}
