/**
* @author José Abel Castellanos Joo - jcastellanos34@gmail.com
*/

import com.signalcollect._

object SSSP extends App {	
  
  val Compute = new MaxFlowGraph
  
  val s = "a"
  val t = "d"

  Compute addBFSVertex("a", None)
  Compute addBFSVertex("b", None)
  Compute addBFSVertex("c", None)
  Compute addBFSVertex("d", None)

  Compute addBFSEdge("a", "b", 4)
  Compute addBFSEdge("b", "c", 3)
  Compute addBFSEdge("c", "d", 5)
  Compute addBFSEdge("a", "d", 5)

  val matrix: Array[Array[Int]]	= Array(Array(0, 16, 13, 0, 0, 0), Array(0, 0, 10, 12, 0, 0), Array(0, 4, 0, 0, 14, 0), Array(0, 0, 9, 0, 0, 20), Array(0, 0, 0, 7, 0, 4), Array(0, 0, 0, 0, 0, 0))
  /*
  for (i <- 0 until matrix.length; j <- 0 until matrix.length; if matrix(i)(j) > 0)
  	println(matrix(i)(j))     
  */
  println(Compute MaxFlow(s, t))

}

class MaxFlowGraph {

	def Printing = {
		for(i <- 0 until Vertex.length)
			println(Vertex(i))
		for(i <- 0 until Edges.length)
			println(Edges(i))
	}

  var Vertex: Array[BFSVertex] = Array()  
  var Edges: Array[(String, BFSEdge)] = Array()  

  def FindEdge (x: (String, String), from: Int = 0, data: Array[(String, BFSEdge)]): Int = 
  	if(x == (data(from)._1, (data(from)._2).targetId)) from
  	else FindEdge(x, from + 1, data)

  def FindVertex(x: String, from: Int = 0, data: Array[BFSVertex]): Int = 
  	if(x == data(from).id) from
  	else FindVertex(x, from + 1, data) 

  // This addBFSVertex method stablishs Symbol "" ********************
  // as initial parent for all Vertex ********************************
  def addBFSVertex(id: String, initialState: Option[Double] = None) = 
    Vertex = Vertex :+ new BFSVertex(id, initialState, "")    

  def addBFSEdge(a: String, b: String, w: Double) = 
  	Edges = Edges :+ (a, new BFSEdge(b, w))

  def MaxFlow(s: String, t: String): Double = {
    var maxflow = 0.0
    var son = t
    // minPathFlow will be used in (*) Trace MinPathFlow   
    var minPathFlow = 0.0    
    // pos will be used to store Finding of the Edges in (**) Upgrade Graph
    var pos = 0
    
    // Import Graph Internally ********************************************
    var graph = GraphBuilder.build
    
    for(i <- 0 until Vertex.length){
    	if(Vertex(i).id == s)
    		Vertex(i).state = Some(0)
    	
    	graph addVertex Vertex(i)
    }

    for(i <- 0 until Edges.length)    	
    	graph addEdge (Edges(i)._1, Edges(i)._2)
    // Import Graph Internally ********************************************

    // Signal/Collect Computation *****************************************
    graph.execute
    graph.shutdown
		// Signal/Collect Computation *****************************************

		// Retrieve a parent Map Internally (source => parent) ****************
		var parentMap: Map[String, String] = (for(i <- 0 until Vertex.length) 
												yield Vertex(i).id -> Vertex(i).parent).toMap
		// Retrieve a parent Map Internally (source => parent) ****************

		// Retrieve a weight Map Internally ((source, target) => weight) ******
		var weightMap: Map[(String, String), Double] = (for(i <- 0 until Edges.length) 
													yield (Edges(i)._1, (Edges(i)._2).targetId) -> (Edges(i)._2).weight ).toMap
		// Retrieve a weight Map Internally ((source, target) => weight) ******

		// (*) Trace MinPathFlow **************************************** 
		var parentSon = parentMap(son)
		minPathFlow = weightMap(parentSon, son)
		while(parentSon != ""){
			if(minPathFlow > weightMap(parentSon, son))
				minPathFlow = weightMap(parentSon, son)
			son = parentSon
			parentSon = parentMap(son)
		}
		maxflow += minPathFlow
		// (*) Trace MinPathFlow ****************************************

		// (**) Update Graph *********************************************
	  son = t
	  parentSon = parentMap(son)
	  while(parentSon != ""){    	
	  	pos = FindEdge((parentSon, son), 0, Edges)
	  	//(parentSon, son) -> w.actual - minPathFlow    	
	  	(Edges(pos)._2).w = (Edges(pos)._2).w - minPathFlow
	  		
	  	if(!(weightMap contains (son, parentSon))){
	  		// Needs to be added
	  		addBFSEdge(son, parentSon, minPathFlow)
	  	}
	  	else{
	  		// Only update weight    		
	  		pos = FindEdge((son, parentSon), 0, Edges)
	  		// (son, parentSon) -> w.actual + minPathFlow
	  		(Edges(pos)._2).w = (Edges(pos)._2).w + minPathFlow
	  	}
	  	son = parentSon
	  	parentSon = parentMap(son)
	  }												
	  for(i <- 	0 until Vertex.length){
    	if(Vertex(i).id == s)
    		Vertex(i).state = Some(0)
    	else
    		Vertex(i).state = None
    }
    Edges = Edges filter (x => (x._2).w > 0)
    Vertex = Vertex map (x => new BFSVertex(x.id, x.state, ""))
		// (**) Update Graph *********************************************
		
		// Repeat ***************************************************************
    // Import Graph Internally ********************************************
    graph = GraphBuilder.build
    
    for(i <- 0 until Vertex.length){    	
    	if(Vertex(i).id == s)
    		Vertex(i).state = Some(0)    	
    	graph addVertex Vertex(i)
    }

    for(i <- 0 until Edges.length)    	
    	graph addEdge (Edges(i)._1, Edges(i)._2)
    // Import Graph Internally ********************************************  

    // Signal/Collect Computation *****************************************
    graph.execute    
    graph.shutdown
		// Signal/Collect Computation *****************************************
		
	  while(Vertex(FindVertex(t, 0 , Vertex)).state != None){	  	
			// Retrieve a parent Map Internally (source => parent) ****************
			parentMap = (for(i <- 0 until Vertex.length) 
														yield Vertex(i).id -> Vertex(i).parent).toMap
			// Retrieve a parent Map Internally (source => parent) ****************

			// Retrieve a weight Map Internally ((source, target) => weight) ******
			weightMap = (for(i <- 0 until Edges.length) 
															yield (Edges(i)._1, (Edges(i)._2).targetId) -> (Edges(i)._2).weight ).toMap
			// Retrieve a weight Map Internally ((source, target) => weight) ******

			// (*) Trace MinPathFlow ****************************************
			son = t
			parentSon = parentMap(son)
			minPathFlow = weightMap(parentSon, son)
			while(parentSon != ""){				
				if(minPathFlow > weightMap(parentSon, son))
					minPathFlow = weightMap(parentSon, son)
				son = parentSon
				parentSon = parentMap(son)
			}
			maxflow += minPathFlow
			// (*) Trace MinPathFlow ****************************************

			// (**) Update Graph *********************************************
	    son = t
	    parentSon = parentMap(son)
	    while(parentSon != ""){    	
	    	pos = FindEdge((parentSon, son), 0, Edges)
	    	//(parentSon, son) -> w.actual - minPathFlow    	
	    	(Edges(pos)._2).w = (Edges(pos)._2).w - minPathFlow
	    		
	    	if(!(weightMap contains (son, parentSon))){
	    		// Need to be added
	    		addBFSEdge(son, parentSon, minPathFlow)
	    	}
	    	else{
	    		// Only update weight    		
	    		pos = FindEdge((son, parentSon), 0, Edges)
	    		//(son, parentSon) -> w.actual + minPathFlow
	    		(Edges(pos)._2).w = (Edges(pos)._2).w + minPathFlow
	    	}
	    	son = parentSon
	    	parentSon = parentMap(son)
	    }

	    for(i <- 	0 until Vertex.length){
	    	if(Vertex(i).id == s)
	    		Vertex(i).state = Some(0)
	    	else
	    		Vertex(i).state = None
	    }
	    Edges = Edges filter (x => (x._2).w > 0)
	    Vertex = Vertex map (x => new BFSVertex(x.id, x.state, ""))
	    // (**) Update Graph *********************************************

	    // Import Graph Internally ********************************************
	    graph = GraphBuilder.build
	    
	    for(i <- 0 until Vertex.length){    	
	    	if(Vertex(i).id == s)
	    		Vertex(i).state = Some(0)    	
	    	graph addVertex Vertex(i)
	    }

    	for(i <- 0 until Edges.length)    	
    		graph addEdge (Edges(i)._1, Edges(i)._2)
    	// Import Graph Internally ********************************************  	    
	    
	    // Signal/Collect Computation *****************************************
	    graph.execute
	    graph.shutdown
			// Signal/Collect Computation *****************************************

			// (**) Update Graph *********************************************
	  }
	  maxflow
  }
}

class BFSVertex(id: String, initialState: Option[Double] = None, var parent: String)
  extends DataFlowVertex(id, initialState) {

  override def toString: String = "Id: " + id + ", Parent: " + parent + ", State: " + state.toString

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

class BFSEdge(t: String, var w: Double) extends OptionalSignalEdge(t) {

  def signal = source.state match {
    case None => None
    case Some(distance: Double) => Some((source, distance + weight))
  }

  override def weight: Double = w

  override def toString: String = "BFSVertex: " + t + " with weight " + w
}