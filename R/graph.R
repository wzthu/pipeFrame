#' @importFrom visNetwork visHierarchicalLayout
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visEdges
#' @importFrom visNetwork visOptions
#' @importFrom magrittr %>%
# @importFrom ggdag dagify
# @importFrom ggdag ggdag
#' @importFrom stats as.formula

setClass(Class = "GraphMng",
         slots = list(edges = "data.frame",
                      allStepNames = "character"),
         prototype = list(edges = NULL,
                          allStepNames = NULL))

setMethod(f = "initialize",
          signature = "GraphMng",
          definition = function(.Object, ...){
              options(stringsAsFactors = FALSE)
              .Object@edges <- data.frame(fromStepType= "BASE",toStepType = "BASE", argOrder = 1)
              options(stringsAsFactors = TRUE)
              allStepNames <- "BASE"
              .Object
          })



setGeneric(name = "graphMngAddEdges",
           def = function(graphMngObj,edges, argOrder,...){
               standardGeneric("graphMngAddEdges")
           })
setMethod(f = "graphMngAddEdges",
          signature = "GraphMng",
          definition = function(graphMngObj,edges, argOrder,...){
              if(length(edges)%%2 == 1){
                  stop("the number of step type in edges should be even")
              }
              if( length(argOrder)> 1){
                  if(length(edges)/2 != length(argOrder)){
                      stop("numbers in argOrder should be same with edges")
                  }
              }
              fromStepType <- edges[seq_len(length(edges))%%2==1]
              toStepType <- edges[seq_len(length(edges))%%2==0]
              options(stringsAsFactors = FALSE)
              newEdges <- data.frame(fromStepType, toStepType, argOrder)
              options(stringsAsFactors = TRUE)
              newEdges <- rbind(graphMngObj@edges, newEdges)

              graphMngObj@edges <- newEdges

              graphMngObj@allStepNames <- unique(c(newEdges$fromStepType, newEdges$toStepType))

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
                                downstreamStep, downstreamArgOrder,...){
              return(sum(graphMngObj@edges$fromStepType == upstreamStep &
                             graphMngObj@edges$toStepType == downstreamStep &
                             graphMngObj@edges$argOrder == downstreamArgOrder) > 0)

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
#' @param stepType \code{Character} scalar.
#' Step class name of each step.
#' @param display \code{Logical} scalar.
#'  Whether show the picture on device or not.
#' @param ... Additional arguments, currently used.
#' @rdname graphMng
#' @return \item{addEdges}{No value will be returned.}
#' @aliases  graphMng
#' @examples
#' addEdges(edges = c("RandomRegionOnGenome",
#'                    "OverlappedRandomRegion"),argOrder = 1)
#' printMap()
#'
#' getPrevSteps("OverlappedRandomRegion",1)
#'
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
getPrevSteps <- function(stepType, argOrder){
    graphMng <- getGraphObj()
    return(graphGetPrevSteps(graphMng,stepType, argOrder))

}

setGeneric(name = "graphGetPrevSteps",
           def = function(graphMngObj,stepType, argOrder,...)
               standardGeneric("graphGetPrevSteps")
)

setMethod(f = "graphGetPrevSteps",
          signature = "GraphMng",
          definition = function(graphMngObj,stepType, argOrder,...){
              prev <- graphMngObj@edges$fromStepType[graphMngObj@edges$toStepType == stepType &
                                                 graphMngObj@edges$argOrder == argOrder ]
              if(length(prev)>0){
                  return(prev)
              }else{
                  return(NULL)
              }
          })


#' @rdname graphMng
#' @return \item{getNextSteps}{Names of next steps}
#' @aliases  getPrevSteps
#' @export
getNextSteps <- function(stepType, argOrder){
    graphMng <- getGraphObj()
    return(graphGetNextSteps(graphMng,stepType, argOrder))

}

setGeneric(name = "graphGetNextSteps",
           def = function(graphMngObj,stepType, argOrder,...)
               standardGeneric("graphGetNextSteps")
)

setMethod(f = "graphGetNextSteps",
          signature = "GraphMng",
          definition = function(graphMngObj,stepType, argOrder,...){
              nexttype <- graphMngObj@edges$toStepType[graphMngObj@edges$fromStepType == stepType &
                                                         graphMngObj@edges$argOrder == argOrder ]
              if(length(nexttype)>0){
                  return(nexttype)
              }else{
                  return(NULL)
              }
          })


#' @rdname graphMng
#' @return \item{printMap}{Print the flow map for the pipeline.}
#' @aliases  printMap
#' @export
printMap <- function(stepType=NULL,display=TRUE,...){
    graphMng <- getGraphObj()
    return(graphPrintMap(graphMng, stepType = stepType, display=display,...))

}


setGeneric(name = "graphPrintMap",
           def = function(graphMngObj,stepType=NULL,display = TRUE,...){
               standardGeneric("graphPrintMap")
           })
setMethod(f = "graphPrintMap",
          signature = "GraphMng",
          definition = function(graphMngObj,stepType=NULL,display=TRUE,...){
              edges <- graphMngObj@edges
              edges <- edges[edges[,1]!="BASE",]
              allStepNames <- graphMngObj@allStepNames
              allStepNames <- allStepNames[allStepNames!='BASE']
              stepId <- seq_len(length(allStepNames))
              names(stepId) <- allStepNames
              nodes <- data.frame(id = stepId,
                label = allStepNames, # add labels on nodes
      #         group = c("GrA", "GrB"),   # add groups on nodes
      #         value = 1:10,              # size adding value
                shape = "ellipse",                   # control shape of nodes
     #          title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),
     # tooltip (html or character)
    #           color = color, # color
                shadow = FALSE                  # shadow
              )
              if(!is.null(stepType)){
                  color <- rep("lightblue",length(stepId))
                  stopifnot(!is.na(stepId[stepType]))
                  color[stepId[stepType]] <- "red"
                  nodes <- data.frame(id = stepId,
                      label = names(stepId), # add labels on nodes
            #         group = c("GrA", "GrB"),  # add groups on nodes
            #         value = 1:10,  # size adding value
                      shape = "ellipse",  # control shape of nodes
            #          title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),
            # tooltip (html or character)
                                      color = color, # color
                                      shadow = FALSE                  # shadow
                  )

              }


              edges <- data.frame(from = stepId[edges$fromStepType],
                                  to = stepId[edges$toStepType],
                #label = paste("Edge", 1:8),    # add labels on edges
                #length = c(100,500),          # length
                                  arrows = "to",            # arrows
                                  dashes = FALSE,           # dashes
                                  # title = paste("Edge", 1:8),
                  # tooltip (html or character)
                                  smooth = FALSE,  # smooth
                                  shadow = FALSE
              )
              visNetwork(nodes, edges, width = "100%") %>%
                  visEdges(arrows = "to",physics = FALSE) %>%
                  visOptions(highlightNearest = list(enabled =TRUE,
                                                     degree = 1))%>%
                  visHierarchicalLayout(sortMethod = "directed",
                                        blockShifting=FALSE)
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



