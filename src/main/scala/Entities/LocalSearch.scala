package Entities

object LocalSearch {

  private var _depthNearestNeighbourList: Integer = null

  def depthNearestNeighbourList = _depthNearestNeighbourList
  def depthNearestNeighbourList_=(depthNearestNeighbourList:Integer) = {
    _depthNearestNeighbourList = depthNearestNeighbourList
  }

}
