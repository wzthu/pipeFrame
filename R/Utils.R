#' @name Utils
#' @rdname  Utils
#' @title functions for directory operations
#' @param filepath \code{character} scalar or vector.
#' @param words \code{character} scalar

#' @return \item{getBasenamePrefix}{Get the step ID}
#' @rdname Utils
#' @aliases getBasenamePrefix
#' @export
getBasenamePrefix <- function(filepath,words,...){
    return(basename(gsub(paste0("[.]",words,".*"),"",filepath)))
}
#' @return \item{getPathPrefix}{Get the step ID}
#' @rdname Utils
#' @aliases getPathPrefix
#' @export
getPathPrefix <- function(filepath,words,...){
    return(gsub(paste0("[.]",words,".*"),"",filepath))
}



#' @return \item{checkFileExist}{(For package developer) Check file is exist.}
#' @rdname Utils
#' @aliases checkFileExist
#' @export
checkFileExist <- function(filePath,...){
    filePath <- unlist(filePath)
    stopifnot(!is.null(filePath))
    for(p in filePath){
        if(!file.exists(p)){
            stop(paste("error, file does not exist:",p))
        }
    }
}

#' @return \item{checkPathExist}{(For package developer) Check directory is exist.}
#' @rdname Utils
#' @aliases checkPathExist
#' @export
checkPathExist <- function(filePath,...){
    filePath <- unlist(filePath)
    stopifnot(!is.null(filePath))
    for(p in filePath){
        if(!dir.exists(dirname(p))){
            stop(paste("error, path does not exist:",p))
        }
    }
}
#' @return \item{checkFileCreatable}{(For package developer) Check file creatable.}
#' @rdname Step-class
#' @aliases checkFileCreatable
#' @export
checkFileCreatable <- function(filePath,...){
    filePaths <- unlist(filePath)
    stopifnot(!is.null(filePaths))
    for(filePath in filePaths){
        if(file.exists(filePath)){
            warning(paste("file exist:",filePath,". It may be overwrited in processing"))
        }else if(!file.create(filePath)){
            stop(paste("cannot create file '",filePath,"', No such file or directory or permission denied"))
        }else{
            unlink(filePath)
        }
    }
}




msgBoxBegin<-function(){
    message(">>>>>>========================================")
}

msgBoxDone<-function(){
    message("========================================<<<<<<")
    message(" ")
}
