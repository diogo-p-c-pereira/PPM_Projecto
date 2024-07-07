package ZigZagGame

import ZigZag.Coord2D

import scala.annotation.tailrec

object Direction extends Enumeration {
  type Direction = Value
  val North, South, East, West,
  NorthEast, NorthWest, SouthEast, SouthWest = Value

  def processCoord(coord: Coord2D, dir: Direction): Coord2D = dir match {
    case North => (coord._1 - 1, coord._2)
    case South => (coord._1 + 1, coord._2)
    case East => (coord._1, coord._2 + 1)
    case West => (coord._1, coord._2 - 1)
    case NorthEast => (coord._1 - 1, coord._2 + 1)
    case NorthWest => (coord._1 - 1, coord._2 - 1)
    case SouthEast => (coord._1 + 1, coord._2 + 1)
    case SouthWest => (coord._1 + 1, coord._2 - 1)
    case _ => coord
  }

  def isContiguous(coord1: Coord2D, coord2: Coord2D): Boolean = {
    @tailrec
    def _isContiguous(coord1: Coord2D, coord2: Coord2D, dirs: List[Direction]): Boolean = dirs match{
      case Nil => false
      case x::xs => if(coord1 == Direction.processCoord(coord2,x)) true else _isContiguous(coord1, coord2, xs)
    }
    _isContiguous(coord1, coord2, Direction.getAllDirs)
  }

  def getAllContiguousCoords(coord2D: Coord2D): List[Coord2D] = {
    def _getAllContiguousCoords(coord2D: Coord2D, dirs: List[Direction]): List[Coord2D] = dirs match {
      case Nil => Nil
      case x::xs => processCoord(coord2D,x)::_getAllContiguousCoords(coord2D, xs)
    }
    _getAllContiguousCoords(coord2D, getAllDirs)
  }

  def getDirection(coord1: Coord2D, coord2: Coord2D): Direction = {
    val vector = (coord2._1-coord1._1, coord2._2-coord1._2)
    vector match{
      case (-1,0) => North
      case (1,0) => South
      case (0,1) => East
      case (0,-1) => West
      case (-1,1) => NorthEast
      case (-1,-1) => NorthWest
      case (1,1) => SouthEast
      case (1,-1) => SouthWest
      case _ => null
    }
  }

  def getAllDirs: List[Direction] = {
    Direction.values.toList
  }
}