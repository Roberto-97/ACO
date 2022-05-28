package Alg

class AcoMasterSlave {

  //  override def evaluateAnts(colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters, sparkContext: Option[SparkContext]): Unit = {
  //    colonie.ants = sparkContext.get.parallelize(colonie.ants).mapPartitions(iterator => {
  //      val tc = TaskContext.get()
  //      tspParameters.randomNumber = new Random(ep.seed * (tc.partitionId() + 1))
  //      iterator.map(ant => {
  //        /* Mark all cities as unvisited*/
  //        ant.initializeVisited(tspParameters.numberCities)
  //        /* Place the ants on same initial city*/
  //        ant.randomInitialPlaceAnt(tspParameters.numberCities, tspParameters.randomNumber)
  //        /* Choose the nexts cities to visit*/
  //        (1 until tspParameters.numberCities).map((step) => neighbourChooseAndMoveToNext(step, ant, colonie, ep, tspParameters))
  //        /* Compute tour length*/
  //        ant.computeTour(tspParameters.numberCities, tspParameters.distance)
  //      })
  //    }).collect().toVector
  //  }

}
