#' @rdname initPipeFrame
#' @title initialize the pipeFrame package
#' @description This function should be used first line in R terminal for general user.
#'  And it shoul be used in .onLoad() function when developing package.
#'  It's main function is to configure the basic options for the package.
#' @param defaultJobName \code{Character} scalar. The default job name for the pacakge.
#' When user uses pipeFrame package, "pipeFrame-pipeline" is the default defaultJobName.
#' @param availableGenome \code{Character} scalar or vector.
#' To configure the default valid genome like "hg19", "mm10", etc. to be configured.
#' @param defaultCheckAndInstallFunc \code{Function} scalar. The function need to call several \code{\link{runWithFinishCheck}}
#' @param defaultThreads \code{Numeric} scalar. The maximun thread a step can use. Default 2
#' @param defaultTmpDir \code{Character} scalar. The directory for  intermediate result storage of all steps. Default: Current woring directory.
#' @param defaultRefDir \code{Character} scalar. The directory for reference data dependency storage. Default: \code{file.path(getwd(),"refdir")}
#' @param defaultReference \code{List} scalar. Reference files list.
#' @return Nothing will be returned.
#' @examples
#' initPipeFrame(availableGenome = c("hg19", "hg38", "mm9", "mm10","testgenome"),
#'               defaultJobName = paste0("pkgname","-pipeline")
#' )
#'
#' @export
initPipeFrame <- function(defaultJobName,
                          availableGenome,
                          defaultCheckAndInstallFunc = NULL,
                          defaultThreads = 2,
                          defaultTmpDir = getwd(),
                          defaultRefDir = file.path(getwd(),"refdir"), #file.path("~",".pipeFrame","refdir"),
                          defaultReference = list(test=list(file="fileName",rc = "obj"))
                          ){
    if(defaultJobName == "pipeFrame-pipeline"){
        oldavailgenome <- c("hg19", "hg38", "mm9", "mm10","testgenome")
    }else{
        oldavailgenome <- getOption("pipeFrameConfig.genome.valid")
    }

    options(pipeFrameConfig.threads = defaultThreads)
    if(!is.null(availableGenome) && !is.null(oldavailgenome)){
        for(i in 1:length(availableGenome)){
            stopifnot(availableGenome[i]%in%c("hg19", "hg38", "mm9", "mm10","testgenome"))
        }
        options(pipeFrameConfig.genome.valid = intersect(availableGenome,oldavailgenome))
    }else{
        options(pipeFrameConfig.genome.valid = NULL)
    }
    allowChange = getOption("pipeFrameConfig.allowChangeJobDir")
    if(is.null(allowChange)){
        allowChange <- TRUE
        options(pipeFrameConfig.allowChangeJobDir = TRUE)
    }
    if(allowChange){
        tryCatch({if(getJobName()=="pipeFrame-pipeline"){
            setJobName(defaultJobName)
        }},
        error = function(cond){
            setJobName(defaultJobName)
        })
        tryCatch(getTmpDir(),
                 error = function(cond){
                     setTmpDir(defaultTmpDir)
                 })
    }
    tryCatch(getRefDir(),
             error = function(cond){
                 setRefDir(defaultRefDir)
             })
    cni<-getOption("pipeFrameConfig.genome.checkAndInstallFunc")
    if(is.null(defaultCheckAndInstallFunc)){
        options(pipeFrameConfig.genome.checkAndInstallFunc = c(cni,checkAndInstall))
    }else{
 #       print(defaultCheckAndInstallFunc)
 #       stopifnot(!is.function(defaultCheckAndInstallFunc))
        options(pipeFrameConfig.genome.checkAndInstallFunc = c(cni,defaultCheckAndInstallFunc))
    }

    options(pipeFrameConfig.genome.refs = defaultReference)

    count <- getOption("pipeFrameConfig.count")
    if(is.null(count)){
        options(pipeFrameConfig.count = 0L)
    }


}

.onLoad <- function(libname, pkgname) {
    initPipeFrame(availableGenome = c("hg19", "hg38", "mm9", "mm10","testgenome"),
                  defaultJobName = paste0(pkgname,"-pipeline")
    )
}




