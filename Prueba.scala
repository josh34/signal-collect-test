import com.signalcollect._

object SSSP extends App {
  val graph = GraphBuilder.build
    graph.addVertex(new Location(1, Some(0)))
    graph.addVertex(new Location(2))
    graph.addVertex(new Location(3))
    graph.addVertex(new Location(4))
    graph.addEdge(1, new Path(2, 4.2))
    graph.addEdge(2, new Path(3, 3.12))
    graph.execute
    graph.foreachVertex(println(_))
    graph.shutdown    
}

class Location(id: Int, initialState: Option[Double] = None)
  extends DataFlowVertex(id, initialState) {
  
  type Signal = Double
  
  var parent: Int = -1

  def collect(signal: Double) = {

  	val newState = state match {  	
   	 	case None => Some(signal)
    	case Some(currentShortestPath) => Some(math.min(currentShortestPath, signal))
  	}

  	newState
  }

}

class Path(t: Int, w: Double) extends OptionalSignalEdge(t) {
  
  def signal = source.state match {
    case None => None
    case Some(distance: Double) => Some(distance + weight)
  }

  override def weight: Double = w

}
