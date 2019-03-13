#' @export
initPipeFrame <- function(defaultJobName,
                          availableGenome,
                          defaultCheckAndInstallFunc = NULL,
                          defaultThreads = 2,
                          defaultTmpDir = getwd(),
                          defaultRefDir = file.path(getwd(),"refdir"), #file.path("~",".pipeFrame","refdir"),
                          defaultReference = list()
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
    if(is.null(defaultCheckAndInstallFunc)){
        options(pipeFrameConfig.genome.checkAndInstallFunc = checkAndInstall)
    }else{
 #       print(defaultCheckAndInstallFunc)
 #       stopifnot(!is.function(defaultCheckAndInstallFunc))
        options(pipeFrameConfig.genome.checkAndInstallFunc = defaultCheckAndInstallFunc)
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




