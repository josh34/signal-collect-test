/**
* @author José Abel Castellanos Joo - jcastellanos34@gmail.com
*/

import com.signalcollect._

object SSSP extends App {	

  def Printing: Unit = {		
    for(i <- 0 until Vertex.length)
      println(Vertex(i))

        for(i <- 0 until Edges.length)
          println(Edges(i)._1 + " " + Edges(i)._2)		
  }

  // Graph Data **********************************************************************************
  var max_flow: Double = 0.0
  val s = 0
  val t = 5 
  var matrixGraph: Array[Array[Double]] = Array(Array(0, 16, 13, 0, 0, 0),
        Array(0, 0, 10, 12, 0, 0),
        Array(0, 4, 0, 0, 14, 0),
        Array(0, 0, 9, 0, 0, 20),
        Array(0, 0, 0, 7, 0, 4),
        Array(0, 0, 0, 0, 0, 0))
  var parent = for{p <- (0 until matrixGraph.length).toArray} yield -1	
  // Graph Data **********************************************************************************

  // Import Graph ********************************************************************************
  var Vertex: Array[BFSVertex] = ( for {p <- 0 until matrixGraph.length} yield 
        if(p == s)  (new BFSVertex(p, Some(0), parent(p))) 
        else		    (new BFSVertex(p, None, parent(p))) ).toArray

  var Edges: Array[(Int, BFSEdge)] = ( for {i <- 0 until matrixGraph.length; 
        j <- 0 until matrixGraph.length; 
        if(matrixGraph(i)(j) > 0)} yield
        (i, new BFSEdge(j, matrixGraph(i)(j))) ).toArray
   
  var graph = GraphBuilder.build

  for(i <- 0 until Vertex.length)
      graph addVertex (Vertex(i))		

  for(i <- 0 until Edges.length)
  	graph addEdge (Edges(i)._1, Edges(i)._2)
  // Import Graph ********************************************************************************

  // Signal/Collect Computation ****************	
  graph.execute  
  // Signal/Collect Computation ****************

  // Update parent ********************
  for(i <- 0 until Vertex.length)
  	parent(i) = Vertex(i).parent
  // Update parent ********************

  // Get Min Path Flow from Node T to Node S ******************	
  var son = t
  var minPathFlow = (Int.MaxValue).toDouble	
  var i = parent(son)

  while(parent(i) >= 0){
  	if(minPathFlow > (Vertex(i).outgoingEdges(son)).weight)
    	minPathFlow = (Vertex(i).outgoingEdges(son)).weight
    son = i
    i = parent(i)
  }

  if(minPathFlow > (Vertex(i).outgoingEdges(son)).weight)
    minPathFlow = (Vertex(i).outgoingEdges(son)).weight	
  max_flow += minPathFlow
  // Get Min Path Flow from Node T to Node S ******************

  // Update Graph *********************************************
  i = t
  var j = parent(i)

  while(parent(i) >= 0){		
  	j = parent(i)
    matrixGraph(j)(i) -= minPathFlow;
    matrixGraph(i)(j) += minPathFlow;
    i = parent(i)
  }

  Vertex = ( for {p <- 0 until matrixGraph.length} yield 
      if(p == s)  (new BFSVertex(p, Some(0), parent(p))) 
      else		    (new BFSVertex(p, None, parent(p))) ).toArray
  Edges = ( for {i <- 0 until matrixGraph.length; 
  		j <- 0 until matrixGraph.length; 
        if(matrixGraph(i)(j) > 0)} yield
        (i, new BFSEdge(j, matrixGraph(i)(j))) ).toArray

  graph = GraphBuilder.build

  for(i <- 0 until Vertex.length)
    graph addVertex (Vertex(i))		

  for(i <- 0 until Edges.length)
  	graph addEdge (Edges(i)._1, Edges(i)._2)
  // Update Graph *********************************************

  // Repeat Again Until No Augmented Path exists ****************************	
  // Signal/Collect Computation ****************	
  graph.execute  
  // Signal/Collect Computation ****************

  // Update parent ********************
  for(i <- 0 until Vertex.length)
  	parent(i) = Vertex(i).parent
  // Update parent ********************

  while(Vertex(t).state != None){
  	// Get Min Path Flow from Node T to Node S ******************	
    son = t
    minPathFlow = (Int.MaxValue).toDouble	
    i = parent(son)

    while(parent(i) >= 0){
    	if(minPathFlow > (Vertex(i).outgoingEdges(son)).weight)
        	minPathFlow = (Vertex(i).outgoingEdges(son)).weight
        son = i
        i = parent(i)
    }

    if(minPathFlow > (Vertex(i).outgoingEdges(son)).weight)
    	minPathFlow = (Vertex(i).outgoingEdges(son)).weight			
    max_flow += minPathFlow
    // Get Min Path Flow from Node T to Node S ******************

    // Update Graph *********************************************
    i = t
    j = parent(i)

    while(parent(i) >= 0){		
    	j = parent(i)
    	matrixGraph(j)(i) -= minPathFlow;
    	matrixGraph(i)(j) += minPathFlow;
    	i = parent(i)
    }

    Vertex = ( for {p <- 0 until matrixGraph.length} yield 
    	if(p == s)  (new BFSVertex(p, Some(0), parent(p))) 
        else		(new BFSVertex(p, None, parent(p))) ).toArray
    Edges = ( for {i <- 0 until matrixGraph.length; 
    	j <- 0 until matrixGraph.length; 
        if(matrixGraph(i)(j) > 0)} yield
        	(i, new BFSEdge(j, matrixGraph(i)(j))) ).toArray

    graph = GraphBuilder.build

    for(i <- 0 until Vertex.length)
    	graph addVertex (Vertex(i))		

    for(i <- 0 until Edges.length)
    	graph addEdge (Edges(i)._1, Edges(i)._2)
    // Update Graph ****************************************************

    // Signal/Collect Computation ****************	
    graph.execute  
    // Signal/Collect Computation ****************

    // Update parent *****************************
    for(i <- 0 until Vertex.length)
    	parent(i) = Vertex(i).parent
    // Update parent *****************************

                }
  // Repeat Again Until No Augmented Path existis ****************************

  println("The Max Flow is: " + max_flow)

  graph.shutdown

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