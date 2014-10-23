#Perform hill-climbing local search to find the best scoring model
gm.search = function(data, graph.init, forward=TRUE, backward=TRUE, score="aic"){
    return(0) #TODO implement
}

#Restart hill-climbing search with randomly generated initial graphs
gm.restart(nstart, prob, seed, data, graph.init, forward=TRUE, backward=TRUE, score="aic")
{
    return(0) #TODO implement
}

#Generate a random graph with given probability that two nodes are connected
graph.random(prob)
{
    return(0) #TODO implement
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
