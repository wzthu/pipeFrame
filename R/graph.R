#' @importFrom visNetwork visHierarchicalLayout
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visEdges
#' @importFrom visNetwork visOptions

#' @importFrom magrittr %>%

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
              graphMngObj@allStepNames <- unique(c(graphMngObj@allStepNames, edges))
              count <- length(graphMngObj@stepIds)
              if(count < length(graphMngObj@allStepNames)){
                  newid <- (count+1):length(graphMngObj@allStepNames)
                  names(newid) <- graphMngObj@allStepNames[is.na(graphMngObj@stepIds[graphMngObj@allStepNames])]
                  graphMngObj@stepIds <- c(graphMngObj@stepIds,newid)
              }

              for(i in 1:length(startPoints)){
                  if(sum(graphMngObj@edgeStarts[[argOrder]] == startPoints[i] &
                         graphMngObj@edgeEnds[[argOrder]] == endPoints[i]) == 0){
                      #print(graphMngObj)
                      graphMngObj@edgeStarts[[argOrder]] <- c(graphMngObj@edgeStarts[[argOrder]],startPoints[i])
                      graphMngObj@edgeEnds[[argOrder]] <- c(graphMngObj@edgeEnds[[argOrder]],endPoints[i])
                  }
              }
              # for (i in 1:length(graphMngObj@edgeStarts)) {
              #     pt <- graphMngObj@edgeStarts[[paste0("edge",i)]]
              #     if(pt[1] == "BASE"){
              #         graphMngObj@edgeStarts[[paste0("edge",i)]] <- pt[2:length(pt)]
              #     }
              #     pt <- graphMngObj@edgeEnds[[paste0("edge",i)]]
              #     if(pt[1] == "BASE"){
              #         graphMngObj@edgeEnds[[paste0("edge",i)]] <- pt[2:length(pt)]
              #     }
              # }
              graphMngObj
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
              #        print(raphMngObj@edgeEnds[[i]][graphMngObj@edgeStarts[[i]] == stepName])
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
                     # print(raphMngObj@edgeStarts[[i]][graphMngObj@edgeEnds[[i]] == stepName])
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
              return(sum(graphMngObj@edgeStarts[[downstreamArgOrder]] == upstreamStep &
                      graphMngObj@edgeEnds[[downstreamArgOrder]] == downstreamStep) > 0)
              })

#' @export
addEdges <- function(edges, argOrder){
    graphMng <- getOption("pipeFrameConfig.graph")
    if(is.null(graphMng)){
        graphMng <- new("GraphMng")
    }
    graphMng <- graphMngAddEdges(graphMng,edges,argOrder)
    options(pipeFrameConfig.graph = graphMng)
}


#' @export
checkRelation<-function(upstreamStep,downstreamStep,downstreamArgOrder){
    graphMng <- getOption("pipeFrameConfig.graph")
    stopifnot(!is.null(graphMng))
    return(graphMngCheckRelation(graphMng,upstreamStep,downstreamStep,downstreamArgOrder))
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


              edges <- data.frame(from = graphMngObj@stepIds[na.omit(unlist(graphMngObj@edgeStarts))],
                                  to = graphMngObj@stepIds[na.omit(unlist(graphMngObj@edgeEnds))],
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

          })



