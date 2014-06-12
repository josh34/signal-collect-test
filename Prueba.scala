import com.signalcollect._

object SSSP extends App {
	
	def Printing: Unit = {
		// Printing **********************************
		for(i <- 0 until BFSVertex.length)
			println(BFSVertex(i))

		for(i <- 0 until BFSEdges.length)
			println(BFSEdges(i)._1 + " " + BFSEdges(i)._2)
		// Printing **********************************
	}

	def ForEach(data: List[Int], f: Int => Unit): Unit = data match {
		case x :: xs => 
			f(x)
			ForEach(xs, f)
		case Nil => 
			()
	}

	// Graph Data **************************************************************************************
	val s = 0
	val t = 3
	var matrixGraph = Array(Array(0, 1, 3, 0), Array(1, 0, 1, 5), Array(3, 1, 0, 3), Array(0, 5, 3, 0))
	var parent = for{p <- (0 until matrixGraph.length).toArray} yield -1

	var BFSVertex: Array[BFSVertex] = ( for {p <- 0 until matrixGraph.length} yield 
																		if(p == s)  (new BFSVertex(p, Some(0), parent(p))) 
																		else		    (new BFSVertex(p, None, parent(p))) ).toArray

	var BFSEdges: Array[(Int, BFSEdge)] = ( for {i <- 0 until matrixGraph.length; 
																				j <- 0 until matrixGraph.length; 
																				if(matrixGraph(i)(j) > 0)} yield
																					(i, new BFSEdge(j, matrixGraph(i)(j))) ).toArray
	// Graph Data **************************************************************************************
	
	val graph = GraphBuilder.build
	// Import Graph ******************************
  for(i <- 0 until BFSVertex.length)
  	graph addVertex (BFSVertex(i))		

	for(i <- 0 until BFSEdges.length)
		graph addEdge (BFSEdges(i)._1, BFSEdges(i)._2)
	// Import Graph ******************************		
	
	//Printing

	// Signal/Collect Computation ****************	
  graph.execute
  graph.shutdown
  // Signal/Collect Computation ****************

  Printing
  /*
  println("Lol " + BFSEdges(0)._1 + " " + BFSEdges(0)._2)
  BFSEdges(0)._2.w = 121.23
	println("Lol " + BFSEdges(0)._1 + " " + BFSEdges(0)._2)
	*/

	// TraceBack from T Node to S Node
	var i = t
	while(parent(i) > 0){
		println	(parent(i))
		i = parent(i)
	}

}

class BFSVertex(id: Int, initialState: Option[Double] = None, var parent: Int)
  extends DataFlowVertex(id, initialState) {
  
  override def toString: String = "Id: " + id.toString + ", Parent: " + parent.toString + ", State: " + state.toString

  type Signal = Tuple2[BFSVertex, Double]
  
  def collect(signal: Signal) = {

		val source = signal._1
  	val signalState = signal._2	

  	val newState = state match {  	
   	 	case None => 
   	 		parent = source.id
   	 		Some(signalState)
    	case Some(currentShortestBFSEdge) => 
    		if(currentShortestBFSEdge > signalState)
    			parent = source.id
    		Some(math.min(currentShortestBFSEdge, signalState))
  	}
  
  	newState
  
  }

}

class BFSEdge(t: Int, var w: Double) extends OptionalSignalEdge(t) {
 
  def signal = source.state match {
    case None => None
    case Some(distance: Double) => Some((source, distance + weight))
  }

  override def weight: Double = w

  override def toString: String = "BFSVertex: " + t + " with weight " + w

}