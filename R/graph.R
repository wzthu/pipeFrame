setClass(Class = "GraphMng",
         slots = list(edgeStarts = "list",
                      edgeEnds = "list",
                      allStepNames = "character"),
         prototype = list(edgeStarts = list(),
                          edgeEnds = list(),
                          allStepNames = NULL))



setGeneric(name = "graphMngAddEdges",
           def = function(graphMngObj,edges, argOrder,...){
               standardGeneric("graphMngAddEdges")
           })
setMethod(f = "graphMngAddEdges",
          signature = "GraphMng",
          definition = function(graphMngObj,edges, argOrder,...){
              stopifnot(length(edges)%%2==1)
              s <- 1:length(edges)
              startPoints <- edges[s%%2 == 1]
              endPoints <- edges[s%%2 == 0]
              graphMngObj@allStepName <- unique(c(graphMngObj@allStepName, edges))
              for(i in length(startPoints)){
                  if(sum(graphMngObj@edgeStarts[[argOrder]] == startPoints[i] &&
                         graphMngObj@edgeEnds[[argOrder]] == endPoints[i]) == 0){
                      graphMngObj@edgeStarts[[argOrder]] <- c(graphMngObj@edgeStarts[[argOrder]],startPoints[i])
                      graphMngObj@edgeEnds[[argOrder]] <- c(graphMngObj@edgeEnds[[argOrder]],endPoints[i])
                  }
              }
          })


setGeneric(name = "graphMngGetNextSteps",
           def = function(graphMngObj,stepName,...){
               standardGeneric("graphMngGetNextSteps")
           })
setMethod(f = "graphMngGetNextSteps",
          signature = "GraphMng",
          definition = function(graphMngObj,stepName,...){
              for(i in 1:length(graphMngObj@edgeStarts)){
                  if(sum(graphMngObj@edgeStarts[[i]] == stepName)==0){
                      message(sprintf("Next steps on arguments %d are not available.",i))
                  }else{
                      message(sprintf("Next steps on arguments %d are available for:",i))
                      print(raphMngObj@edgeEnds[[i]][graphMngObj@edgeStarts[[i]] == stepName])
                  }
              }
          })

setGeneric(name = "graphMngGetPrevSteps",
           def = function(graphMngObj,stepName,...){
               standardGeneric("graphMngGetPrevSteps")
           })
setMethod(f = "graphMngGetPrevSteps",
          signature = "GraphMng",
          definition = function(graphMngObj,stepName,...){
              for(i in 1:length(graphMngObj@edgeEnds)){
                  if(sum(graphMngObj@edgeEnds[[i]] == stepName)==0){
                      message(sprintf("Previous steps on arguments %d are not available.",i))
                  }else{
                      message(sprintf("Previous steps on arguments %d are available for:",i))
                      print(raphMngObj@edgeStarts[[i]][graphMngObj@edgeEnds[[i]] == stepName])
                  }
              }
          })


setGeneric(name = "graphMngCheckRelation",
           def = function(graphMngObj, upstreamStep,downstreamStep,downstreamArgOrder,...){
               standardGeneric("graphMngCheckRelation")
           })
setMethod(f = "graphMngCheckRelation",
          signature = "GraphMng",
          definition = function(graphMngObj, upstreamStep,downstreamStep,downstreamArgOrder,...){
              return(sum(graphMngObj@edgeStarts[[downstreamArgOrder]] == startPoints[i] &&
                      graphMngObj@edgeEnds[[downstreamArgOrder]] == endPoints[i]) > 0)
              })


addEdges <- function(edges, argOrder){
    graphMng <- getOption("pipeFrameConfig.graph")
    if(is.null(graphMng)){
        graphMng <- new("GraphMng")
        graphMngAddEdges(graphMng,edges,argOrder)
    }
    options(pipeFrameConfig.graph = graphMng)
}



checkRelation<-function(upstreamStep,downstreamStep,downstreamArgOrder){
    graphMng <- getOption("pipeFrameConfig.graph")
    stopifnot(!is.null(graphMng))
    return(graphMngCheckRelation(graphMng,upstreamStep,downstreamStep,downstreamArgOrder))
}

#
#
# setClass(Class = "Test",
#          slots = list(testslot = "list"),
#          prototype = list(edgeStarts = list())
#                           )
# setGeneric(name = "testg0",
#            def = function(.Objects,...){
#                standardGeneric("testg0")
#            })
#
# setMethod("testg0", signature="Test",definition =  function(.Objects,...){testg(.Objects)})
#
#
# setGeneric(name = "testg",
#            def = function(.Objects,...){
#                standardGeneric("testg")
#            })

