#coronary.dat <- read.table("C:/Users/joey/Documents/GitHub/DataMiningA2/coronary.txt")
#TODO remove
printDebug = function(s)
{
  #print(s)
}

#Perform hill-climbing local search to find the best scoring model
gm.search = function(data, graph.init, forward=TRUE, backward=TRUE, score="aic", doPrint=TRUE){
  #Set up
  currentGraph = graph.init
  
  numNodes = length(data[1,])
  numObs = length(data[,1])
  
  
  #Max number of params is the df of the mutual-independence model plus the # params in that model
  #Is fast to compute
  indLL = loglin(table(data), getCliques(matrix(0,numNodes, numNodes)), param=TRUE)
  maxParams = indLL$df + length(indLL$param)
  
  printDebug("Max params")
  printDebug(maxParams)
  
  currentScore = getScore(data, numObs, currentGraph, score, maxParams)
  
  optimalFound = FALSE
  
  
  nextTrace = 1
  trace = list()
  
  
  
  #loop:
  numIters = 0
  while(!optimalFound) #Help debug infinite loops
  {
    printDebug("In search loop")
    
    numIters = numIters + 1
    
    #Find the neighbours of the current graph
    #allNeighbours = getNeighbours(currentGraph)
    
    
    #Find the score for each neighbour
    #scores = 1:(length(allNeighbours))
    #for (i in 1:length(allNeighbours))
    #{
      #scores[i] = getScore(data, numObs, allNeighbours[[i]], score)
    #}
    bestNeighbourData = bestNeighbour(data, numObs, currentGraph, score, maxParams)
    neighbourScore = bestNeighbourData$bestScore
    neighbourChange = bestNeighbourData$bestPair
    
    #bestScoreIndex = which.min(scores)
    #printDebug("All scores")
    #printDebug(scores)
    #printDebug(bestScoreIndex)
    #printDebug(allNeighbours[[bestScoreIndex]])
    
    #If lowest neighbour is lower than current, then it's the new current
    if ( neighbourScore < currentScore)
    {
      i = neighbourChange[1]
      j = neighbourChange[2]
      added = neighbourChange[3]
      diffString = differenceString(i,j,added, neighbourScore)
      if (doPrint)
      {
        print(diffString)
      }
      
      trace[nextTrace] = diffString
      nextTrace = nextTrace + 1
      
      #currentGraph = allNeighbours[[bestScoreIndex]]
      currentGraph[i,j] = !currentGraph[i,j]
      currentGraph[j,i] = !currentGraph[j,i]
      currentScore = neighbourScore
      #currentScore = scores[bestScoreIndex]
      
      
    }
    #Else we're done
    else{
      optimalFound = TRUE
    }
    
    
    
  }
  
  #TODO here: return model, score, trace and call
  return(list(model = getCliques(currentGraph),
              score = currentScore,
              trace = trace, #TODO fill this in
              call = match.call()))
  
  
}

differenceString <- function(i, j, added, score)
{

      if (added == 1)
      {
        return(paste("Added ", i, " - ", j, "(score = ", score, ")") )
      }
      if (added == 0)
      {
        return(paste("Removed ", i, " - ", j, "(score = ", score, ")") )
      }
    
  
}

#Constants for adding or removing nodes


bestNeighbour = function(data, numObs, graph, score, maxParams)
{
  numNodes = length(graph[1,])
  bestScore = 1/0 #Start as positive infinity
  bestPair = c(-1, -1)
  
  for (i in 1:numNodes)
  {
    for (j in 1:(i-1))
    {
      
      #Modify our graph
      graph[i,j] =  !graph[i,j]
      graph[j,i] =  !graph[j,i]

      graphScore = getScore(data, numObs, graph, score, maxParams)
      if (graphScore < bestScore)
      {
        bestPair = c(i,j, graph[i,j])
        bestScore = graphScore
      }
      
      #Return graph to its old state
      graph[i,j] =  !graph[i,j]
      graph[j,i] =  !graph[j,i]
    }
  }
  return (list(bestPair=bestPair, bestScore=bestScore))
}

# getNeighbours = function(graph)
# {
#   printDebug("Get neighbours")
#   numNodes = length(graph[1,])
#   printDebug("Get neighbours with size")
#   printDebug(numNodes)
#   numNeighbours = (numNodes*(numNodes-1)/2) #Formula for sum(1..n, i-1)
#   neighbours = matrix(-1, numNeighbours, 2)
#   
#   for (i in 1:numNodes)
#   {
#     for (j in 1:(i-1))
#     {
#       #TODO what about FORWARD and BACKWARD?
#       
#       newGraph = graph #TODO is this a copy
#       newGraph[i,j] =  !newGraph[i,j]
#       newGraph[j,i] =  !newGraph[j,i]
#       #Add the new graph to our list
#       
#       printDebug("Neighbour size")
#       printDebug(length(neighbours))
#       
#       #neighbours[[length(neighbours) + 1]] <- newGraph
#       neighbours[]
#     }
#   }
#   return (neighbours) #TODO implement
# }

getScore <- function(data, numObs, graph, score, maxParams)
{
  cliques = getCliques(graph)
  
  printDebug("Getting score of graph:")
  printDebug(graph)
  printDebug("Num nodes")
  printDebug(length(graph[1,]))
  
  
  
  #printDebug(data)
  printDebug("Cliques")
  printDebug(cliques)
  
  loglinResult = loglin(table(data), cliques, print=FALSE)

  printDebug("Finished loglin")
  deviance = loglinResult$lrt
  
  numParams = maxParams - loglinResult$df
  #numParams = length(loglinResult$param) #TODO how to get num params?
  #print("Num params")
  #print(numParams)
  
  
  if (score == "aic")
  {
    return(deviance + 2*numParams)
  }
  else if (score == "bic")
  {
    printDebug("BIC score")
    printDebug(numObs)
    return(deviance + log(numObs)*numParams)
  }
  
}

#Restart hill-climbing search with randomly generated initial graphs
gm.restart <- function(nstart, prob, seed, data, forward=TRUE, backward=TRUE, score="aic")
{
  #set our random seed
  set.seed(seed)
  
  #make sure the best is always really high to start
  bestScoreSoFar = 1000000000000000
  bestModel = 0
  
  #Get the number of data points
  n = dim(data)[2]
  
  for (i in 1:nstart)
  {
    printDebug("Starting new search")
    graph = graph.random(prob, n)
    model = gm.search(data, graph, forward, backward, score)
    if (bestScoreSoFar > model$score){ #model score is lower than best so far
      bestModel = model
      bestScoreSoFar = model$score
    }
  }
  
  return(bestModel) #TODO implement
}

#Generate a random graph with given probability that two nodes are connected
graph.random <- function(prob, numNodes)
{
  printDebug("Num nodes in random graph")
  printDebug(numNodes)
  m = matrix(0,numNodes, numNodes)
  for (i in 1:numNodes)
  {
    for (j in 1:i)
    {
      rand <- runif(1, 0.0, 1.0)
      if (rand <= prob && i != j)
      {
        m[i,j] = 1
        m[j,i] = 1
      }
    }
  }
  return(m)
}

getCliques <- function(gr)
{
  n = dim(gr)[1]  
  
  
  return(find.cliques(c(), 1:n, c(), gr, list() ) )
}

#Clique code provided for us
find.cliques <- function (R,P,X,graph,cliques) 
{
  if (length(P)==0 & length(X)==0) {cliques <- list(R)}
  else {
    pivot <- P[sample(length(P),1)]
    for(i in 1:length(P)){
      pivot.nb <- neighbors(graph,pivot)
      if(!is.element(P[i],pivot.nb)){
        P.temp <- setdiff(P,P[i])
        R.new <- union(R,P[i])
        P.new <- intersect(P.temp,neighbors(graph,P[i]))
        X.new <- intersect(X,neighbors(graph,P[i]))
        cliques <- c(cliques,find.cliques(R.new,P.new,X.new,graph,list()))
        X <- union(X,P[i])}
    }}
  cliques
}


neighbors <- function (graph,node) 
{
  nnodes <- dim(graph)[2]
  (1:nnodes)[graph[node,]==1]
}

post.process <- function (cliques) 
{
  unique(lapply(cliques,sort))
}
