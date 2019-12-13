#' @name Utils
#' @rdname  Utils
#' @title Functions for directory operations
#' @param filepath \code{character} scalar or vector.
#' @param words \code{character} scalar.
#' Remove substring of the path characters starting to match the word
#' @param filePath \code{Character} scalar.
#' @param ... Additional arguments, currently unused
#' @examples
#' getBasenamePrefix("aaa/bbb.ccc.ddd","cCc")
#'
#' getBasenamePrefix("aaa/bbb.ccc.ddd","ddd")
#'
#' getPathPrefix("aaa/bbb.ccc.ddd","dDd")
#'
#' getPathPrefix("aaa/bbb.ccc.ddd","ccc")
#'
#' file.create("test.bed")
#'
#' checkFileExist("test.bed")
#'
#' tryCatch({checkFileExist("test.bed1")},error = function(e) e)
#'
#' dir.create("testdir")
#'
#' checkPathExist(file.path(getwd(),"testdir"))
#'
#' tryCatch({checkPathExist(file.path(dirname(getwd()),
#' "notexistfolder","testdir"))},error = function(e) e)
#'
#' checkFileCreatable("aaa.bed")
#'
#' tryCatch({checkFileCreatable("testdir1/aaa.bed")},error = function(e) e)
#' @return \item{getBasenamePrefix}{Get the filepath
#' basename with removed suffix}
#' @aliases getBasenamePrefix
#' @export
getBasenamePrefix <- function(filepath,words,...){
    return(basename(gsub(paste0("[.](",words,").*"),"",filepath,
                         ignore.case = TRUE)))
}
#' @return \item{getPathPrefix}{Get the filepath  with removed suffix}
#' @rdname Utils
#' @aliases getPathPrefix
#' @export
getPathPrefix <- function(filepath,words,...){
    return(gsub(paste0("[.]",words,".*"),"",filepath, ignore.case = TRUE))
}



#' @return \item{checkFileExist}{(For package developer) Check file is exist.}
#' @rdname Utils
#' @aliases checkFileExist
#' @export
checkFileExist <- function(filePath,...){
    filePath <- unlist(filePath)
    stopifnot(!is.null(filePath))
    # for(p in filePath){
    #     if(!file.exists(p)){
    #         stop(paste("error, file does not exist:",p))
    #     }
    # }
    lapply(filePath, function(p){
        if(!file.exists(p)){
            stop(paste("error, file does not exist:",p))
        }
    })
}

#' @return \item{checkPathExist}{(For package developer)
#' Check directory is exist.}
#' @rdname Utils
#' @aliases checkPathExist
#' @export
checkPathExist <- function(filePath,...){
    filePath <- unlist(filePath)
    stopifnot(!is.null(filePath))
    # for(p in filePath){
    #     if(!dir.exists(dirname(p))){
    #         stop(paste("error, path does not exist:",p))
    #     }
    # }
    lapply(filePath, function(p){
        if(!dir.exists(dirname(p))){
            stop(paste("error, path does not exist:",p))
        }
    })
}
#' @return \item{checkFileCreatable}{(For package developer)
#' Check file creatable.}
#' @rdname Utils
#' @aliases checkFileCreatable
#' @export
checkFileCreatable <- function(filePath,...){
    filePaths <- unlist(filePath)
    stopifnot(!is.null(filePaths))
    # for(filePath in filePaths){
    #     if(file.exists(filePath)){
    #         warning(paste("file exist:",filePath,
    #                       ". It may be overwrited in processing"))
    #     }else if(!file.create(filePath)){
    #         stop(paste("cannot create file '",filePath,
    #                    "', No such file or directory or permission denied"))
    #     }else{
    #         unlink(filePath)
    #     }
    # }
    lapply(filePaths, function(filePath){
        if(file.exists(filePath)){
            warning(paste("file exist:",filePath,
                          ". It may be overwrited in processing"))
        }else if(!file.create(filePath)){
            stop(paste("cannot create file '",filePath,
                       "', No such file or directory or permission denied"))
        }else{
            unlink(filePath)
        }
    })
}



#' @return \item{addFileSuffix}{(For package developer) Check if file suffix existed and add suffix}
#' @rdname Utils
#' @aliases addFileSuffix
#' @export
addFileSuffix <- function(filePath, suffix, ...){
    stopifnot(!is.null(filePath))
    if(!startsWith(suffix,".")){
        suffix <- paste0(".", suffix)
    }
    if(suffix == "."){
        suffix <- ""
    }
    rs <- lapply(filePath, function(p){
        if(endsWith(filePath, suffix)){
            return(p)
        }else{
            return(paste0(p, suffix))
        }
    })
    if(is.list(filePath)){
        return(rs)
    }else{
        return(unlist(rs))
    }
}



#' @name loadStep
#' @rdname loadStep
#' @title load step object from rds file
#' @description load PipeFrame Step (or its inherit class) object from rds file
#' @param rdsfile \code{Character} scalar. The rds file directory for Step  (or its inherit class) Object
#' @param regClass \code{Logical} scalar. Register the Class of object to inherit from Step Class.
#' Default: TRUE. Note: make sure corresponding packages depending on pipeFrame is loaded
#' @return Step (or its inherit class) object
#' @examples
#' objrds <- system.file(package = "pipeFrame", "extdata","pipeFrame.obj.rds")
#' obj <- loadStep(objrds)
#'
#' @export
loadStep <- function(rdsfile, regClass = TRUE){
    obj <- readRDS(rdsfile)
    if(regClass){
        regAttachedStep(class(obj)[1],obj@stepBaseClass)
    }
    return(obj)

}


msgBoxBegin<-function(){
    message(">>>>>>==========================================================")
}

msgBoxDone<-function(){
    message("==========================================================<<<<<<")
    message(" ")
}
