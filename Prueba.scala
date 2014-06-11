import com.signalcollect._

object SSSP extends App {
  val graph = GraphBuilder.build
    graph addVertex(new Location(1, Some(0)))
    graph addVertex(new Location(2))
    graph addVertex(new Location(3))
    graph addVertex(new Location(4))
    graph addEdge(1, new Path(2, 4.2))
    graph addEdge(2, new Path(3, 3.12))
    graph.execute
    
    graph foreachVertex (println(_))
    

    graph.shutdown    
}

class Location(id: Int, initialState: Option[Double] = None)
  extends DataFlowVertex(id, initialState) {
  
  override def toString: String = "Id: " + id.toString + " Parent:" + parent.toString

  type Signal = Tuple2[Double, Location]
  
  var parent: Int = -1

  def collect(signal: Signal) = {

	val source = signal._2
    val signalState = signal._1    

  	val newState = state match {  	
   	 	case None => 
   	 		parent = source.id
   	 		Some(signalState)
    	case Some(currentShortestPath) => 
    		parent = source.id
    		Some(math.min(currentShortestPath, signalState))
  	}

  	newState
  }

}

class Path(t: Int, w: Double) extends OptionalSignalEdge(t) {
  
  def signal = source.state match {
    case None => None
    case Some(distance: Double) => Some((distance + weight, source))
  }

  override def weight: Double = w

}
