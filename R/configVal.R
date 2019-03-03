
getValidGenome <- function(){
    getOption("pipeFrameConfig.genome.valid")
}

getCheckAndInstallFunc <- function(){
    getOption("pipeFrameConfig.genome.checkAndInstallFunc")
}

setGenome <- function(genome, check = TRUE, ...){
  genome <- match.arg(genome,getValidGenome())
  if(!dir.exists(file.path(getRefDir(),genome))){
      dir.create(file.path(getRefDir(),genome))
  }

  options(pipeFrameConfig.genome = genome)
  print(checkAndInstall)
  checkAndInstall <- getCheckAndInstallFunc()
  print(checkAndInstall)
  checkAndInstall(check, ...)
}


getGenome <- function(){
  genome <- getOption("pipeFrameConfig.genome")
  if(is.null(genome)){
    stop("please call `setGenome` to set genome first")
  }
  return(genome)
}


setRefDir <- function(refdir, createDir = TRUE)
{
    if(!dir.exists(refdir)){
        if(createDir){
            dir.create(refdir,recursive = TRUE)
        }else{
            stop(paste("path does not exist:",refdir))
        }
    }
    options(pipeFrameConfig.refdir = refdir)
}

getRefDir <- function(){
    refdir <- getOption('pipeFrameConfig.refdir')
    stopifnot(dir.exists(refdir))
    return(refdir)
}


setTmpDir <- function(tmpDir = getwd()){
    stopifnot(dir.exists(tmpDir))
    allowChange <- getOption("pipeFrameConfig.allowChangeJobDir")
    if(!allowChange){
        stop("setTmpDir must be called before calling any pipeling steps")
    }
    options(pipeFrameConfig.dir.tmpdir = tmpDir)
}

getTmpDir <- function(){
    tmpDir <- getOption("pipeFrameConfig.dir.tmpdir")
    stopifnot(dir.exists(tmpDir))
    return(tmpDir)
}


setJobName <- function(jobName){
    stopifnot(is.character(jobName))
    allowChange <- getOption("pipeFrameConfig.allowChangeJobDir")
    if(!allowChange){
        stop("setJobName must be called before calling any pipeling steps")
    }
    options(pipeFrameConfig.dir.jobname = jobName)
}

getJobName <- function(){
    jobName <- getOption("pipeFrameConfig.dir.jobname")
    stopifnot(is.character(jobName))
    return(jobName)
}

getJobDir <- function(){
    tmpDir <- getTmpDir()
    jobName <- getJobName()
    jobDir <- file.path(tmpDir,jobName)
    if(!dir.exists(jobDir)){
        dir.create(jobDir)
    }
    return(jobDir)
}



getRefFilePath <- function(fileName,check = TRUE){
  genome <- getGenome()
  refdir <- getRefDir()
  reffilepath<-file.path(refdir,genome,fileName)
  if(check){
    stopifnot(file.exists(reffilepath))
  }
  return(reffilepath)
}


runWithFinishCheck <- function(func, refName, resultDirPaths = NULL, resultVal = NULL, execForNonRsFile = TRUE){
    message(paste("Configure",refName,"..."))

    if(!is.null(resultDirPaths)){
        stopifnot(is.character(resultDirPaths))
    }
    resultDirPaths <- file.path(getRefDir(),getGenome(),resultDirPaths)
    lockFilePath <- file.path(getRefDir(),getGenome(),paste(refName,"lock",sep = "."))
    if(file.exists(lockFilePath)){
        unlink(resultDirPaths,recursive = TRUE, force = TRUE)
        unlink(lockFilePath,recursive = TRUE, force = TRUE)
    }
    rc <- NULL
    if(!is.null(resultDirPaths)){
        print(file.exists(resultDirPaths))
        if(prod(file.exists(resultDirPaths)) < 0.5){
            file.create(lockFilePath)
            rc <- func(resultDirPaths)
        }
    }else if(exec){
        file.create(lockFilePath)
        rc <- func(resultDirPaths)
    }
    if(!is.null(resultVal)){
        rc <- resultVal
    }
    if(!is.null(resultDirPaths)){
        stopifnot(file.exists(resultDirPaths))
    }
    unlink(lockFilePath,recursive = TRUE, force = TRUE)
    refs <- getOption("pipeFrameConfig.genome.refs")
    stopifnot(is.list(refs))
    refs[[refName]] <- list(files = resultDirPaths, rc = rc)
    options(pipeFrameConfig.genome.refs=refs)
    message(paste("Configure",refName,"finished"))
}


getRef <- function(refName){
    refs <- getOption("pipeFrameConfig.genome.refs")
    ref<-refs[[refName]]
    stopifnot(!is.null(ref))
    return(ref)
}

getRefFiles <- function(refName){
    ref <- getRef(refName = refName )
    return(ref$files)
}

getRefRc <- function(refName){
    ref <- getRef(refName = refName )
    return(ref$rc)
}





checkAndInstall <- function(check = TRUE, ...){
}

#checkAndInstall <- function(check = TRUE, ...){
#    runWithFinishCheck(func = checkAndInstallBSgenome,refName = "bsgenome", resultVal = getBSgenome(genome), execForNonRsFile = check)
#    runWithFinishCheck(func = checkAndInstallGenomeFa,refName = "fasta", resultDirPaths = file.path(getRefDir(),getGenome(),paste0(getGenome(),".fa")))
#}


