#' @importFrom BSgenome getBSgenome
#' @importFrom GenomeInfoDb seqnames
#' @importFrom Biostrings masks
#' @importFrom Biostrings DNAStringSet
#' @importFrom Biostrings injectHardMask
#' @importFrom Biostrings writeXStringSet

#'

#' @name setGenome
#' @rdname setGenome
#' @title Configure genome for all steps
#' @description Configure the reference genome assembly for all steps.
#' @param genome \code{Character} scalar. Valid genome to be configured.
#' @return \item{getValidGenome}{\code{Character} scalar.
#' All valid genome assemblies for this package.}
#' @aliases getValidGenome
#' @rdname setGenome
#' @examples
#' getValidGenome()
#' setGenome("hg19")
#' getGenome()
#'
#' @export
getValidGenome <- function(){
    getOption("pipeFrameConfig.genome.valid")
}


getCheckAndInstallFunc <- function(){
    getOption("pipeFrameConfig.genome.checkAndInstallFunc")
}

#' @return \item{setGenome}{All packages and dependencies are
#' configured and installed. No value will be returned.}
#' @aliases setGenome
#' @rdname setGenome
#' @export
setGenome <- function(genome){
    genome <- match.arg(genome,getValidGenome())
    tryCatch(getRefDir(),
             error = function(cond){
                     setRefDir(file.path(getTmpDir(),"refdir"))
             })
    if(!dir.exists(file.path(getRefDir(),genome))){
        dir.create(file.path(getRefDir(),genome))
    }

    options(pipeFrameConfig.genome = genome)
    #  print(checkAndInstall)
    checkAndInstall <- getCheckAndInstallFunc()
    #  print(checkAndInstall)
    for(cai in checkAndInstall){
        cai()
    }
}

#' @return \item{getGenome}{\code{Character} scalar.
#' Display the configured genome.}
#' @aliases getGenome
#' @rdname setGenome
#' @export
getGenome <- function(){
    genome <- getOption("pipeFrameConfig.genome")
    if(is.null(genome)){
        stop("please call `setGenome` to set genome first")
    }
    return(genome)
}

#' @importFrom parallel detectCores

#' @name setThreads
#' @rdname setThreads
#' @title Configure the maximum number of threads
#' @description Configure the maximum number of threads for all steps
#' @param threads \code{Numeric} scalar. The maximum number of
#' threads that can be allocated  to each step.
#' @return \item{setThreads}{No value will be returned}
#' @aliases setThreads
#' @rdname setThreads
#' @examples
#' setThreads()
#' getThreads()
#' @export
setThreads <- function(threads = detectCores()){
    stopifnot(is.numeric(threads))
    options(pipeFrameConfig.threads = threads)
}

#' @return \item{getThreads}{\code{Numeric} scalar.
#' Display the maximum number of threads that
#' can be allocated  to each step.}
#' @aliases getThreads
#' @rdname setThreads
#' @export
getThreads <- function(){
    return(getOption("pipeFrameConfig.threads"))

}

#' @name setRefDir
#' @rdname setRefDir
#' @title Set the reference directory
#' @param refdir \code{Character} scalar.
#' The directory to store the reference data.
#' @param refName \code{Character} scalar.
#' The name of reference data.
#' @param createDir \code{Logica} scalar.
#' Create the directory if the directory does not exist. Default: TRUE
#' @return \item{setRefDir}{No value will be returned}
#' @aliases setRefDir
#' @rdname setRefDir
#' @examples
#' setRefDir("./refdir")
#' getRefDir()
#' getRef("test")
#'
#' getRefFiles("test")
#' getRefRc("test")
#'
#' @export
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
#' @return \item{getRefDir}{\code{Character} scalar.
#' Display the directory of reference data.}
#' @aliases getRefDir
#' @rdname setRefDir
#' @export
getRefDir <- function(){
    refdir <- getOption('pipeFrameConfig.refdir')
    stopifnot(dir.exists(refdir))
    return(refdir)
}


#' @return \item{getRef}{\code{List} scalar. A list object
#' which contains "files" (reference file paths) and "rc" (reference R object)}
#' @aliases getRef
#' @rdname setRefDir
#' @export
getRef <- function(refName){
    refs <- getOption("pipeFrameConfig.genome.refs")
    ref<-refs[[refName]]
    stopifnot(!is.null(ref))
    return(ref)
}

#' @return \item{getRefFiles}{\code{Character} scalar or vector.
#' Display the reference file directory. }
#' @aliases getRefFiles
#' @rdname setRefDir
#' @export
getRefFiles <- function(refName){
    ref <- getRef(refName = refName )
    return(ref$files)
}

#' @return \item{getRefRc}{\code{Uncertain} scalar or vector.
#' Display any reference R object. }
#' @aliases getRefRc
#' @rdname setRefDir
#' @export
getRefRc <- function(refName){
    ref <- getRef(refName = refName )
    return(ref$rc)
}

#
# getRefFilePath <- function(fileName,check = TRUE){
#     genome <- getGenome()
#     refdir <- getRefDir()
#     reffilepath<-file.path(refdir,genome,fileName)
#     if(check){
#         stopifnot(file.exists(reffilepath))
#     }
#     return(reffilepath)
# }


#' @name setTmpDir
#' @rdname setTmpDir
#' @title Configure the directory for intermediate results of all steps
#' @param tmpDir \code{Character} scalar.
#' The directory to store intermediate results of all steps.
#' Default: Current directory.
#' @return \item{setTmpDir}{No value will be returned}
#' @aliases setTmpDir
#' @rdname setTmpDir
#' @examples
#' setTmpDir()
#' getTmpDir()
#' @export
setTmpDir <- function(tmpDir = getwd()){
    stopifnot(dir.exists(tmpDir))
    # allowChange <- getOption("pipeFrameConfig.allowChangeJobDir")
    # if(!allowChange){
    #     stop("setTmpDir must be called before calling any pipeling steps")
    # }
    options(pipeFrameConfig.dir.tmpdir = tmpDir)
}

#' @return \item{getTmpDir}{\code{Character} scalar.
#' Display the directory for intermediate results of all steps.}
#' @aliases getTmpDir
#' @rdname setTmpDir
#' @export
getTmpDir <- function(){
    tmpDir <- getOption("pipeFrameConfig.dir.tmpdir")
    stopifnot(dir.exists(tmpDir))
    return(tmpDir)
}

#' @name setJobName
#' @rdname setJobName
#' @title Configure the job name for following steps.
#' @param jobName \code{Character} scalar. Job name for following steps.
#' @return \item{setJobName}{No value will be returned}
#' @aliases setJobName
#' @rdname setJobName
#' @examples
#' setJobName("testJobName")
#' getJobName()
#' getJobDir()
#' @export
setJobName <- function(jobName){
    stopifnot(is.character(jobName))
    #    allowChange <- getOption("pipeFrameConfig.allowChangeJobDir")
    #   if(!allowChange){
    #       stop("setJobName must be called before calling any pipeling steps")
    #    }
    options(pipeFrameConfig.dir.jobname = jobName)
}
#' @return \item{setJobName}{Set a job name for following steps.}
#' @aliases getJobName
#' @rdname setJobName
#' @export
getJobName <- function(){
    jobName <- getOption("pipeFrameConfig.dir.jobname")
    stopifnot(is.character(jobName))
    return(jobName)
}
#' @return \item{setJobName}{Set the job directory}
#' @aliases getJobName
#' @rdname setJobName
#' @export
getJobDir <- function(){
    tmpDir <- getTmpDir()
    jobName <- getJobName()
    jobDir <- file.path(tmpDir,jobName)
    if(!dir.exists(jobDir)){
        dir.create(jobDir)
    }
    return(jobDir)
}




#' @name runWithFinishCheck
#' @rdname runWithFinishCheck
#' @title Install dependent data or software with finishing check
#' @param func \code{Function} scalar.
#' The function with refFilePath argument (reference file directory).
#' The returned value will be set as the reference object.
#' @param refName \code{Character} scalar.
#' Reference name for \code{\link{getRef}},
#' \code{\link{getRefFiles}} and \code{\link{getRefRc}}.
#' @param refFilePath \code{Character} scalar.
#' The reference file relative directory under the "refdir/genome/"
#' @param genome \code{Character} scalar.
#'  The genome like "hg19". Default: getGenome()
#' @return \item{runWithFinishCheck}{No value will be returned}
#' @aliases runWithFinishCheck
#' @rdname runWithFinishCheck
#' @examples
#'
#' checkAndInstall <- function(){
#'    runWithFinishCheck(func = checkAndInstallBSgenome,refName = "bsgenome")
#'    runWithFinishCheck(func = checkAndInstallGenomeFa,refName = "fasta",
#'    refFilePath = paste0(getGenome(),".fa"))
#' }
#' initPipeFrame(availableGenome = c("hg19", "hg38","mm9","mm10","testgenome"),
#'               defaultJobName = paste0("pkgname","-pipeline")
#' )
#'
#' setGenome("hg19")
#'
#' @export
runWithFinishCheck <- function(func, refName, refFilePath = NULL){
    message(paste("Configure",refName,"..."))
    objPath <- file.path(getRefDir(),getGenome(),paste0(refName,".obj.Rdata"))
    if(is.null(refFilePath)){
        refFilePath <- objPath
    }else{
        stopifnot(is.character(refFilePath))
        refFilePath <- file.path(getRefDir(),getGenome(),refFilePath)
    }

    lockFilePath <- file.path(getRefDir(),getGenome(),
                              paste(refName,"lock",sep = "."))
    if(file.exists(lockFilePath)){
        unlink(refFilePath,recursive = TRUE, force = TRUE)
        unlink(lockFilePath,recursive = TRUE, force = TRUE)
        unlink(objPath,force = TRUE)
    }
    rc <- NULL

    if(prod(file.exists(refFilePath)) < 0.5){
        file.create(lockFilePath)
        if(refFilePath == objPath){
            rc <- func(NULL)
        }else{
            rc <- func(refFilePath)
        }
        save(rc = rc, file = objPath)
        if(getGenome()!="testgenome"){
            Sys.sleep(3)
        }
    }else{
        load(objPath)
    }
    stopifnot(file.exists(refFilePath))
    stopifnot(file.exists(objPath))
    unlink(lockFilePath,recursive = TRUE, force = TRUE)
    refs <- getOption("pipeFrameConfig.genome.refs")
    stopifnot(is.list(refs))
    refs[[refName]] <- list(files = refFilePath, rc = rc)
    options(pipeFrameConfig.genome.refs=refs)
    message(paste("Configure",refName,"finished"))
}






#' @return \item{checkAndInstallBSgenome}{check if there is
#' the BSgenome package installed for
#' curent genome and install it if not. No value will be returned.}
#' @aliases checkAndInstallBSgenome
#' @rdname runWithFinishCheck
#' @export
checkAndInstallBSgenome <- function(refFilePath, genome =  getGenome()){
    bsgenomename<-
        BSgenome::available.genomes()[grepl(paste0(genome,"$"),
                                            BSgenome::available.genomes())]
    if(length(bsgenomename)==0){
        message()
        stop("There is no BSgenome support for this genome")
    }
    bsgenomeinstall <-
        BSgenome::installed.genomes()[grepl(paste0(genome,"$"),
                                            BSgenome::installed.genomes())]
    if(length(bsgenomeinstall)==0){
        message(paste("BSgenome for ",genome,"has not been installed,"))
        message("begin to install ...")
        BiocManager::install(bsgenomename)
    }
}



#' @return \item{checkAndInstallOrgDb}{check if there is the OrgDb
#' package installed for
#' curent genome and install it if not. No value will be returned.}
#' @aliases checkAndInstallOrgDb
#' @rdname runWithFinishCheck
#' @export
checkAndInstallOrgDb <- function(refFilePath, genome =  getGenome()){
    curOrgDb <- NULL
    if(genome == "hg19"||genome == "hg38"){
        curOrgDb <- "org.Hs.eg.db"
    }else if(genome == "mm10" || genome == "mm9"){
        curOrgDb <- "org.Mm.eg.db"
    }else if(genome == "danRer10"){
        curOrgDb <- "org.Dr.eg.db"
    }else if(genome == "galGal5" || genome == "galGal4"){
        curOrgDb <- "org.Gg.eg.db"
    }else if(genome == "rheMac3" || genome == "rheMac8"){
        curOrgDb <- "org.Mmu.eg.db"
    }else if(genome == "panTro4" ){
        curOrgDb <- "org.Pt.eg.db"
    }else if(genome == "rn6" || genome == "rn5"){
        curOrgDb <- "org.Rn.eg.db"
    }else if(genome == "sacCer3" || genome == "sacCer2"){
        curOrgDb <- "org.Sc.sgd.db"
    }else if(genome == "susScr3"){
        curOrgDb <- "org.Ss.eg.db"
    }else {
        stop(paste0("OrgDb Annotation package does not support for ",genome))
        return(NULL)
    }
    tryCatch({
        library(curOrgDb,character.only = TRUE)
    },
    error = function(e){
        BiocManager::install(curOrgDb)
        library(curOrgDb,character.only = TRUE)
    })
    return(curOrgDb)
}



#' @return \item{checkAndInstallTxDb}{check if there is the TxDb
#' package installed for
#' curent genome and install it if not. Nothing will be returned.}
#' @aliases checkAndInstallTxDb
#' @rdname runWithFinishCheck
#' @export
checkAndInstallTxDb <- function(refFilePath, genome =  getGenome()){
    genome <- getGenome()
    curTxDb <- NULL
    if(genome == "hg19"){
        curTxDb <- "TxDb.Hsapiens.UCSC.hg19.knownGene"
    }else if(genome == "hg38"){
        curTxDb <- "TxDb.Hsapiens.UCSC.hg38.knownGene"
    }else if(genome == "mm9"){
        curTxDb <- "TxDb.Mmusculus.UCSC.mm9.knownGene"
    }else if(genome == "mm10"){
        curTxDb <- "TxDb.Mmusculus.UCSC.mm10.knownGene"
    }else if(genome == "danRer10"){
        curTxDb <- "TxDb.Drerio.UCSC.danRer10.refGene"
    }else if(genome == "galGal5"){
        curTxDb <- "TxDb.Ggallus.UCSC.galGal5.refGene"
    }else if(genome == "galGal4"){
        curTxDb <- "TxDb.Ggallus.UCSC.galGal4.refGene"
    }else if(genome == "rheMac3"){
        curTxDb <- "TxDb.Mmulatta.UCSC.rheMac3.refGene"
    }else if(genome == "rheMac8"){
        curTxDb <- "TxDb.Mmulatta.UCSC.rheMac8.refGene"
    }else if(genome == "rn5"){
        curTxDb <- "TxDb.Rnorvegicus.UCSC.rn5.refGene"
    }else if(genome == "rn6"){
        curTxDb <- "TxDb.Rnorvegicus.UCSC.rn6.refGene"
    }else if(genome == "sacCer3"){
        curTxDb <- "TxDb.Scerevisiae.UCSC.sacCer3.sgdGene"
    }else if(genome == "sacCer2"){
        curTxDb <- "TxDb.Scerevisiae.UCSC.sacCer2.sgdGene"
    }else if(genome == "susScr3"){
        curTxDb <- "TxDb.Sscrofa.UCSC.susScr3.refGene"
    }else {
        stop(paste0("TxDb Annotation package does not support for ",genome))
        return(NULL)
    }
    tryCatch({
        library(curTxDb,character.only = TRUE)
    },
    error = function(e){
        BiocManager::install(curTxDb)
        library(curTxDb,character.only = TRUE)
    })
    return(curTxDb)
}


#' @return \item{checkAndInstallGenomeFa}{check if genome
#' FASTA file exist and install if not. No value will be returned}
#' @aliases checkAndInstallGenomeFa
#' @rdname runWithFinishCheck
#' @export
checkAndInstallGenomeFa <- function(refFilePath){
    outFile <- refFilePath
    bsgenome<-getRefRc("bsgenome")
    if(!is(bsgenome, "BSgenome")){
        stop("The variable 'bsgenome' is not a BSgenome")
    }
    append <- FALSE
    for(chrT in seqnames(bsgenome)){
        if(is.null(masks(bsgenome[[chrT]])))
            chrSeq <- DNAStringSet(bsgenome[[chrT]])
        else
            chrSeq <- DNAStringSet(injectHardMask(bsgenome[[chrT]],letter="N"))
        names(chrSeq) <- chrT
        writeXStringSet(chrSeq, filepath=outFile, format="fasta",append=append)
        append <- TRUE
    }
    return(outFile)
}

checkAndInstall <- function(){
}

#checkAndInstall <- function(check = TRUE, ...){
#    runWithFinishCheck(func = checkAndInstallBSgenome,refName = "bsgenome",
#                    resultVal = getBSgenome(genome), execForNonRsFile = check)
#    runWithFinishCheck(func = checkAndInstallGenomeFa,refName = "fasta",
#  refFilePath = file.path(getRefDir(),getGenome(),paste0(getGenome(),".fa")))
#}


