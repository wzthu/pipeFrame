#' @rdname initPipeFrame
#' @title initialize the pipeFrame package
#' @description This function should be called first in R terminal for general users. And it should be used in .onLoad() function for package developers. In this function, several parameters need to be defined and configured, including genome, job name, reference directory, temporary directory, check and install function, threads number, reference list, etc.
#' @param defaultJobName \code{Character} scalar. The default job name for the package. When users use pipeFrame package, defaultJobName is "pipeFrame-pipeline".
#' @param availableGenome \code{Character} scalar or vector.
#' Configure the available valid genome such as "hg19", "mm10", etc.
#' @param defaultCheckAndInstallFunc \code{Function} scalar. The function needs to call  \code{\link{runWithFinishCheck}}
#' @param defaultThreads \code{Numeric} scalar. The maximum thread limit for each step. Default:2
#' @param defaultTmpDir \code{Character} scalar. The directory of intermediate results for all steps. Default: Current working directory.
#' @param defaultRefDir \code{Character} scalar. The directory of reference data. Default: \code{file.path(getwd(),"refdir")}
#' @param defaultReference \code{List} scalar. List of reference files.
#' @return No value will be returned.
#' @examples
#' initPipeFrame(availableGenome = c("hg19", "hg38", "mm9", "mm10"),
#'               defaultJobName = paste0("pkgname","-pipeline")
#' )
#'
#' @export
initPipeFrame <- function(defaultJobName,
                          availableGenome = c("hg19",
                                              "hg38",
                                              "mm9",
                                              "mm10",
                                              "danRer10",
                                              "galGal5",
                                              "galGal4",
                                              "rheMac3",
                                              "rheMac8",
                                              "panTro4",
                                              "rn5",
                                              "rn6",
                                              "sacCer2",
                                              "sacCer3",
                                              "susScr3",
                                              "testgenome"),
                          defaultCheckAndInstallFunc = NULL,
                          defaultThreads = 2,
                          defaultTmpDir = getwd(),
                          defaultRefDir = file.path(getwd(),"refdir"), #file.path("~",".pipeFrame","refdir"),
                          defaultReference = list(test=list(file="fileName",rc = "obj"))
){
    if(defaultJobName == "pipeFrame-pipeline"){
        oldavailgenome <- c("hg19",
                            "hg38",
                            "mm9",
                            "mm10",
                            "danRer10",
                            "galGal5",
                            "galGal4",
                            "rheMac3",
                            "rheMac8",
                            "panTro4",
                            "rn5",
                            "rn6",
                            "sacCer2",
                            "sacCer3",
                            "susScr3",
                            "testgenome")
    }else{
        oldavailgenome <- getOption("pipeFrameConfig.genome.valid")
    }

    options(pipeFrameConfig.threads = defaultThreads)
    if(!is.null(availableGenome) && !is.null(oldavailgenome)){
        for(i in 1:length(availableGenome)){
            stopifnot(availableGenome[i]%in%c("hg19",
                                              "hg38",
                                              "mm9",
                                              "mm10",
                                              "danRer10",
                                              "galGal5",
                                              "galGal4",
                                              "rheMac3",
                                              "rheMac8",
                                              "panTro4",
                                              "rn5",
                                              "rn6",
                                              "sacCer2",
                                              "sacCer3",
                                              "susScr3",
                                              "testgenome"))
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
                 if(!file.exists("DESCRIPTION")){
                     setRefDir(defaultRefDir)
                 }
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
    initPipeFrame(defaultJobName = paste0(pkgname,"-pipeline"))
}




