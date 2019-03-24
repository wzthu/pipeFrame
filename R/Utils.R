#' @name Utils
#' @rdname  Utils
#' @title Functions for directory operations
#' @param filepath \code{character} scalar or vector.
#' @param words \code{character} scalar. Remove substring of the path characters starting to match the word
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
#' checkPathExist("testdir")
#'
#' tryCatch({checkPathExist("testdir1")},error = function(e) e)
#'
#' checkFileCreatable("aaa.bed")
#'
#' tryCatch({checkFileCreatable("testdir1/aaa.bed")},error = function(e) e)
#'
#'
#' @return \item{getBasenamePrefix}{Get the filepath  basename with removed suffix}
#' @rdname Utils
#' @aliases getBasenamePrefix
#' @export
getBasenamePrefix <- function(filepath,words,...){
    return(basename(gsub(paste0("[.]",words,".*"),"",filepath, ignore.case = TRUE)))
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
#' @rdname Utils
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
