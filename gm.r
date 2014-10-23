#Perform hill-climbing local search to find the best scoring model
gm.search = function(data, graph.init, forward=TRUE, backward=TRUE, score="aic"){
  #Set up
  
  #loop:
  
  #Find the neighbours of the current graph
  #Find the score for each neighbour
  #If lowest neighbour is lower than current, then it's the new current
  #Else we're done
  
  
  return(0) #TODO implement
}

getNeighbours = function(graph)
{
  return (0) #TODO implement
}

getScore <- function(graph, score)
{
  if (score == "aic")
  {
    #TODO AIC
  }
  else if (score == "bic")
  {
    #TODO BIC
  }
  return(0) #TODO implement
}

#Restart hill-climbing search with randomly generated initial graphs
gm.restart <- function(nstart, prob, seed, data, graph.init, forward=TRUE, backward=TRUE, score="aic")
{
  return(0) #TODO implement
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
