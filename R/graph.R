#' @importFrom visNetwork visHierarchicalLayout
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visEdges
#' @importFrom visNetwork visOptions
#' @importFrom magrittr %>%
# @importFrom ggdag dagify
# @importFrom ggdag ggdag
#' @importFrom stats as.formula

setClass(Class = "GraphMng",
         slots = list(edgeStarts = "list",
                      edgeEnds = "list",
                      allStepNames = "character",
                      stepIds = "numeric"),
         prototype = list(edgeStarts = list(),
                          edgeEnds = list(),
                          allStepNames = NULL,
                          stepIds = numeric()))



setGeneric(name = "graphMngAddEdges",
           def = function(graphMngObj,edges, argOrder,...){
               standardGeneric("graphMngAddEdges")
           })
setMethod(f = "graphMngAddEdges",
          signature = "GraphMng",
          definition = function(graphMngObj,edges, argOrder,...){
              if(length(graphMngObj@edgeStarts) < argOrder){
                  for (i in (length(graphMngObj@edgeStarts)+1):argOrder) {
                      graphMngObj@edgeStarts[[paste0("edge",i)]]<-"BASE"
                      graphMngObj@edgeEnds[[paste0("edge",i)]]<-"BASE"
                  }
              }
              stopifnot(length(edges)%%2!=1)
              s <- 1:length(edges)
              startPoints <- edges[s%%2 == 1]
              endPoints <- edges[s%%2 == 0]
              graphMngObj@allStepNames <-
                  unique(c(graphMngObj@allStepNames, edges))
              count <- length(graphMngObj@stepIds)
              if(count < length(graphMngObj@allStepNames)){
                  newid <- (count+1):length(graphMngObj@allStepNames)
                  names(newid) <- graphMngObj@allStepNames[
                      is.na(graphMngObj@stepIds[graphMngObj@allStepNames])]
                  graphMngObj@stepIds <- c(graphMngObj@stepIds,newid)
              }

              for(i in 1:length(startPoints)){
                  if(sum(graphMngObj@edgeStarts[[argOrder]] == startPoints[i] &
                         graphMngObj@edgeEnds[[argOrder]] == endPoints[i])==0){
                      #print(graphMngObj)
                      graphMngObj@edgeStarts[[argOrder]] <-
                          c(graphMngObj@edgeStarts[[argOrder]],startPoints[i])
                      graphMngObj@edgeEnds[[argOrder]] <-
                          c(graphMngObj@edgeEnds[[argOrder]],endPoints[i])
                  }
              }
              graphMngObj
          })




setGeneric(name = "graphMngCheckRelation",
           def = function(graphMngObj, upstreamStep,
                          downstreamStep,downstreamArgOrder,...){
               standardGeneric("graphMngCheckRelation")
           })
setMethod(f = "graphMngCheckRelation",
          signature = "GraphMng",
          definition = function(graphMngObj, upstreamStep,
                                downstreamStep,downstreamArgOrder,...){
              return(sum(graphMngObj@edgeStarts[[downstreamArgOrder]] ==
                             upstreamStep &
                             graphMngObj@edgeEnds[[downstreamArgOrder]] ==
                             downstreamStep) > 0)
          })
#' @name graphMng
#' @title Step graph management
#' @description The step relations are managed and restricted
#' to directed acyclic graph. The direction of data flow is
#' from upstream to downstream. So when users create a new step object,
#' restricting its relation with existing steps is necessary.
#' @rdname graphMng
#' @param edges \code{Character} vector.
#' Contain the names of start and end points for all edges.
#' It needs to follow the format like c("startpt1","endpt1","startpt2",
#' "endpt2","startpt3","endpt3").
#' @param argOrder \code{Numeric} scalar.
#'  The argument order of the input Step object.
#' @param stepName \code{Character} scalar.
#' Step class name of each step.
#' @param display \code{Logical} scalar.
#'  Whether show the picture on device or not.
#' @param ... Additional arguments, currently used.
#' @rdname graphMng
#' @return \item{addEdges}{No value will be returned.}
#' @aliases  graphMng
#' @export
addEdges <- function(edges, argOrder){
    graphMng <- getOption("pipeFrameConfig.graph")
    if(is.null(graphMng)){
        graphMng <- new("GraphMng")
    }
    graphMng <- graphMngAddEdges(graphMng,edges,argOrder)
    options(pipeFrameConfig.graph = graphMng)
}

getGraphObj <- function(){
    graphMng <- getOption("pipeFrameConfig.graph")
    stopifnot(!is.null(graphMng))
    return(graphMng)
}


checkRelation<-function(upstreamStep,downstreamStep,downstreamArgOrder){
    graphMng <- getGraphObj()
    return(graphMngCheckRelation(graphMng,upstreamStep,
                                 downstreamStep,downstreamArgOrder))
}

#' @rdname graphMng
#' @return \item{getPrevSteps}{Names of previous steps}
#' @aliases  getPrevSteps
#' @export
getPrevSteps <- function(stepName, argOrder){
    graphMng <- getGraphObj()
    return(graphGetPrevSteps(graphMng,stepName, argOrder))

}

setGeneric(name = "graphGetPrevSteps",
           def = function(graphMngObj,stepName, argOrder,...)
               standardGeneric("graphGetPrevSteps")
)

setMethod(f = "graphGetPrevSteps",
          signature = "GraphMng",
          definition = function(graphMngObj,stepName=NULL,argOrder,...){
              if(length(graphMngObj@edgeStarts) < argOrder){
                  return(NULL)
              }
              prev <- graphMngObj@edgeStarts[[argOrder]][
                  stepName==graphMngObj@edgeEnds[[argOrder]]]
              if (length(prev)==0){
                  return(NULL)
              }else{
                  return(prev)
              }
          })


#' @rdname graphMng
#' @return \item{getNextSteps}{Names of next steps}
#' @aliases  getPrevSteps
#' @export
getNextSteps <- function(stepName, argOrder){
    graphMng <- getGraphObj()
    return(graphGetNextSteps(graphMng,stepName, argOrder))

}

setGeneric(name = "graphGetNextSteps",
           def = function(graphMngObj,stepName, argOrder,...)
               standardGeneric("graphGetNextSteps")
)

setMethod(f = "graphGetNextSteps",
          signature = "GraphMng",
          definition = function(graphMngObj,stepName=NULL,argOrder,...){
              if(length(graphMngObj@edgeEnds) < argOrder){
                  return(NULL)
              }
              nextpt <- graphMngObj@edgeEnds[[argOrder]][
                  stepName==graphMngObj@edgeStarts[[argOrder]]]
              if (length(nextpt)==0){
                  return(NULL)
              }else{
                  return(nextpt)
              }
          })


#' @rdname graphMng
#' @return \item{printMap}{Print the flow map for the pipeline.}
#' @aliases  printMap
#' @export
printMap <- function(stepName=NULL,display=TRUE,...){
    graphMng <- getGraphObj()
    return(graphPrintMap(graphMng, stepName = stepName, display=display,...))

}


setGeneric(name = "graphPrintMap",
           def = function(graphMngObj,stepName=NULL,display = TRUE,...){
               standardGeneric("graphPrintMap")
           })
setMethod(f = "graphPrintMap",
          signature = "GraphMng",
          definition = function(graphMngObj,stepName=NULL,display=TRUE,...){
                          nodes <- data.frame(id = graphMngObj@stepIds,
                                              label = names(graphMngObj@stepIds),                                 # add labels on nodes
                                              #         group = c("GrA", "GrB"),                                     # add groups on nodes
                                              #         value = 1:10,                                                # size adding value
                                              shape = "ellipse",                   # control shape of nodes
                                              #          title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),         # tooltip (html or character)
                                              #           color = color, # color
                                              shadow = FALSE                  # shadow
                          )
                          if(!is.null(stepName)){
                              color <- rep("lightblue",length(graphMngObj@stepIds))
                              stopifnot(!is.na(graphMngObj@stepIds[stepName]))
                              color[graphMngObj@stepIds[stepName]] <- "red"
                              nodes <- data.frame(id = graphMngObj@stepIds,
                                                  label = names(graphMngObj@stepIds),                                 # add labels on nodes
                                                  #         group = c("GrA", "GrB"),                                     # add groups on nodes
                                                  #         value = 1:10,                                                # size adding value
                                                  shape = "ellipse",                   # control shape of nodes
                                                  #          title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),         # tooltip (html or character)
                                                             color = color, # color
                                                  shadow = FALSE                  # shadow
                              )

                          }


                          edges <- data.frame(from = na.omit(graphMngObj@stepIds[na.omit(unlist(graphMngObj@edgeStarts))]),
                                              to = na.omit(graphMngObj@stepIds[na.omit(unlist(graphMngObj@edgeEnds))]),
              #                                label = paste("Edge", 1:8),                                 # add labels on edges
              #                                length = c(100,500),                                        # length
                                              arrows = "to",            # arrows
                                              dashes = FALSE,                                    # dashes
               #                               title = paste("Edge", 1:8),                                 # tooltip (html or character)
                                              smooth = FALSE,                                    # smooth
                                              shadow = FALSE
                            )
                          visNetwork(nodes, edges, width = "100%") %>%
                              visEdges(arrows = "to",physics = FALSE) %>%
                              visOptions(highlightNearest = list(enabled =TRUE, degree = 1))%>%
                             visHierarchicalLayout(sortMethod = "directed",blockShifting=FALSE)
              # from <- names(na.omit(graphMngObj@stepIds[na.omit(
              #     unlist(graphMngObj@edgeStarts))]))
              # to <- names(na.omit(graphMngObj@stepIds[na.omit(
              #     unlist(graphMngObj@edgeEnds))]))
              # edges <-lapply(1:length(from), function(x){
              #     return(as.formula(paste(from[x], "~", to[x])))
              # })
              # allnames <- graphMngObj@allStepNames
              # names(allnames) <- allnames
              # wholedag<-do.call(dagify,c(edges,list(labels = allnames)))
              # aa<-ggdag(wholedag,text = FALSE, use_labels = "label")
              # return(aa)
          })



