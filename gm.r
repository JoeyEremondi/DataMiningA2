#Perform hill-climbing local search to find the best scoring model
gm.search = function(data, graph.init, forward=TRUE, backward=TRUE, score="aic"){
  #Set up
  currentGraph = graph.init
  currentScore = 10000000000000000
  n = length(data[0,])
  optimalFound = FALSE
  
  #loop:
  while(!optimalFound)
  {
    #Find the neighbours of the current graph
    allNeighbours = getNeighbours(currentGraph)
    
    
    #Find the score for each neighbour
    scores = 1:(length(allNeighbours))
    for (i in 1:length(allNeighbours))
    {
      scores[i] = getScore(data, length(graph[,1]), allNeighbours[[i]], score)
    }
    
    bestScoreIndex = which.min(scores)
    
    #If lowest neighbour is lower than current, then it's the new current
    if (scores[bestScoreIndex] < currentScore)
    {
      currentGraph = allNeighbours[[bestScoreIndex]]
      currentScore = scores[bestScoreIndex]
    }
    #Else we're done
    else{
      optimalFound = TRUE
    }
    
    #TODO here: return model, score, trace and call
    return(list(model = getCliques(graph),
                score = currentScore,
                trace = list(), #TODO fill this in
                call = match.call()))
    
  }
  
  
  
  
  return(0) #TODO implement
}

getNeighbours = function(graph)
{
  print("Get neighbours")
  n = length(graph[,1])
  neighbours = list()
  for (i in 1:n)
  {
    for (j in 1:(i-1))
    {
      newGraph = graph #TODO is this a copy
      newGraph[i,j] =  !newGraph[i,j]
      newGraph[j,i] =  !newGraph[j,i]
      #Add the new graph to our list
      neighbours[[length(neighbours) + 1]] <- newGraph
    }
  }
  return (neighbours) #TODO implement
}

getScore <- function(data, n, graph, score)
{
  cliques = getCliques(graph)
  print(data)
  print("Cliques")
  print(cliques)
  
  loglinResult = loglin(table(data), cliques)
  deviance = loclinResult$lrt
  if (score == "aic")
  {
    return(deviance + 2*length(cliques))
  }
  else if (score == "bic")
  {
    return(deviance + log(n)*length(cliques))
  }
  return(0) #TODO implement
}

#Restart hill-climbing search with randomly generated initial graphs
gm.restart <- function(nstart, prob, seed, data, forward=TRUE, backward=TRUE, score="aic")
{
  #make sure the best is always really high to start
  bestScoreSoFar = 1000000000000000
  bestModel = 0
  
  #Get the number of data points
  n = dim(data)[1]
  
  for (i in 1:nstart)
  {
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
