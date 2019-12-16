context("test class Step's operation")


test_that("test basic operation of class Step ",{
    library(BSgenome)
    library(rtracklayer)
    library(magrittr)

    # generate new Step : RandomRegionOnGenome
    setClass(Class = "RandomRegionOnGenome",
             contains = "Step"
    )

    setMethod(
        f = "init",
        signature = "RandomRegionOnGenome",
        definition = function(.Object,prevSteps = list(),...){
            # All arguments in function randomRegionOnGenome
            # will be passed from "..."
            # so get the arguments from "..." first.
            allparam <- list(...)
            sampleNumb <- allparam[["sampleNumb"]]
            regionLen <- allparam[["regionLen"]]
            genome <- allparam[["genome"]]
            outputBed <- allparam[["outputBed"]]
            # no previous steps for this step so ingnore the "prevSteps"
            # begin to set input parameters
            # no input for this step
            # begin to set output parameters
            if(is.null(outputBed)){
                output(.Object)$outputBed <-
                    getStepWorkDir(.Object,"random.bed")
            }else{
                output(.Object)$outputBed <- outputBed
            }
            # begin to set other parameters
            param(.Object)$regionLen <-  regionLen
            param(.Object)$sampleNumb <- sampleNumb
            if(is.null(genome)){
                param(.Object)$bsgenome <-  getBSgenome(getGenome())
            }else{
                param(.Object)$bsgenome <-  getBSgenome(genome)
            }
            # don't forget to return .Object
            .Object
        }
    )

    setMethod(
        f = "processing",
        signature = "RandomRegionOnGenome",
        definition = function(.Object,...){
            # All arguments are set in .Object
            # so we can get them from .Object
            allparam <- list(...)
            sampleNumb <- getParam(.Object,"sampleNumb")
            regionLen <- getParam(.Object,"regionLen")
            bsgenome <- getParam(.Object,"bsgenome")
            outputBed <- getParam(.Object,"outputBed")
            # begin the calculation
            chrlens <-seqlengths(bsgenome)
            selchr <- grep("_|M",names(chrlens),invert=TRUE)
            chrlens <- chrlens[selchr]
            startchrlens <- chrlens - regionLen
            spchrs <- sample(x = names(startchrlens),
                             size =  sampleNumb, replace = TRUE,
                             prob = startchrlens / sum(startchrlens))
            gr <- GRanges()
            for(chr in names(startchrlens)){
                startpt <- sample(x = 1:startchrlens[chr],
                                  size = sum(spchrs == chr),replace = FALSE)
                gr <- c(gr,GRanges(seqnames = chr,
                                   ranges = IRanges(start = startpt, width = 1000)))
            }
            result <- sort(gr,ignore.strand=TRUE)
            rtracklayer::export.bed(object = result, con =  outputBed)
            # don't forget to return .Object
            .Object
        }
    )

    setMethod(
        f = "genReport",
        signature = "RandomRegionOnGenome",
        definition = function(.Object, ...){
            .Object
        }
    )

    # This function is exported in NAMESPACE for user to use
    randomRegionOnGenome <- function(sampleNumb, regionLen = 1000,
                                     genome = NULL, outputBed = NULL, ...){
        allpara <- c(list(Class = "RandomRegionOnGenome", prevSteps = list()),
                     as.list(environment()),list(...))
        step <- do.call(new,allpara)
        invisible(step)
    }


    # generate another new Step : OverlappedRandomRegion
    setClass(Class = "OverlappedRandomRegion",
             contains = "Step"
    )

    setMethod(
        f = "init",
        signature = "OverlappedRandomRegion",
        definition = function(.Object,prevSteps = list(),...){
            # All arguments in function overlappedRandomRegion and
            # runOerlappedRandomRegion will be passed from "..."
            # so get the arguments from "..." first.
            allparam <- list(...)
            inputBed <- allparam[["inputBed"]]
            randomBed <- allparam[["randomBed"]]
            outputBed <- allparam[["outputBed"]]
            # inputBed can obtain from previous step object when running
            # runOerlappedRandomRegion
            if(length(prevSteps)>0){
                prevStep <- prevSteps[[1]]
                input(.Object)$randomBed <-  getParam(prevStep,"outputBed")
            }
            # begin to set input parameters
            if(!is.null(inputBed)){
                input(.Object)$inputBed <- inputBed
            }
            if(!is.null(randomBed)){
                input(.Object)$randomBed <- randomBed
            }
            # begin to set output parameters
            # the output is recemended to set under the step work directory
            if(!is.null(outputBed)){
                output(.Object)$outputBed <-  outputBed
            }else{
                output(.Object)$outputBed <-
                    getAutoPath(.Object, getParam(.Object, "inputBed"),
                                "bed", suffix = "bed")
                # the path can also be generate in this way
                # ib <- getParam(.Object,"inputBed")
                # output(.Object)$outputBed <-
                #    file.path(getStepWorkDir(.Object),
                #    paste0(substring(ib,1,nchar(ib)-3), "bed"))
            }
            # begin to set other parameters
            # no other parameters
            # don't forget to return .Object


            .Object
        }
    )
    setMethod(
        f = "processing",
        signature = "OverlappedRandomRegion",
        definition = function(.Object,...){
            # All arguments are set in .Object
            # so we can get them from .Object
            allparam <- list(...)
            inputBed <- getParam(.Object,"inputBed")
            randomBed <- getParam(.Object,"randomBed")
            outputBed <- getParam(.Object,"outputBed")

            # begin the calculation
            gr1 <- import.bed(con = inputBed)
            gr2 <- import.bed(con = randomBed)
            gr <- second(findOverlapPairs(gr1,gr2))
            export.bed(gr,con = outputBed)
            # don't forget to return .Object
            .Object
        }
    )

    setMethod(
        f = "genReport",
        signature = "OverlappedRandomRegion",
        definition = function(.Object, ...){
            .Object
        }
    )


    # This function is exported in NAMESPACE for user to use
    overlappedRandomRegion <- function(inputBed, randomBed,
                                       outputBed = NULL, ...){
        allpara <- c(list(Class = "OverlappedRandomRegion",
                          prevSteps = list()),as.list(environment()),list(...))
        step <- do.call(new,allpara)
        invisible(step)
    }

    setGeneric("runOverlappedRandomRegion",
               function(prevStep,
                        inputBed,
                        randomBed = NULL,
                        outputBed = NULL,
                        ...) standardGeneric("runOverlappedRandomRegion"))



    setMethod(
        f = "runOverlappedRandomRegion",
        signature = "Step",
        definition = function(prevStep,
                              inputBed,
                              randomBed = NULL,
                              outputBed = NULL,
                              ...){
            allpara <- c(list(Class = "OverlappedRandomRegion",
                              prevSteps = list(prevStep)),as.list(environment()),list(...))
            step <- do.call(new,allpara)
            invisible(step)
        }
    )

    # add to graph
    addEdges(edges = c("RandomRegionOnGenome","OverlappedRandomRegion"),
             argOrder = 1)
    graphObj <- getOption("pipeFrameConfig.graph")
    # begin to test pipeline
    setGenome("hg19")
    expect_equal(getGenome(),"hg19")
    # generate test BED file
    test_bed <- file.path(tempdir(),"test.bed")
    library(rtracklayer)
    export.bed(GRanges("chr7:1-127473000"),test_bed)


    rd <- randomRegionOnGenome(10000)
    overlap <- runOverlappedRandomRegion(rd, inputBed = test_bed)

    randombed <- getParam(rd,"outputBed")

    expect_true(file.exists(randombed))


    overlap1 <-
        overlappedRandomRegion(inputBed = test_bed, randomBed = randombed)

    expect_true(file.exists(getParam(overlap1,"outputBed")))


    overlap1 <- clearStepCache(overlap1)

    expect_false(isReady(overlap1))

    overlap1 <-
        overlappedRandomRegion(inputBed = test_bed, randomBed = randombed)
    rd <- clearStepCache(rd)
    expect_false(isReady(rd))

    overlap1 <- clearStepCache(overlap1)

    expect_false(isReady(overlap1))
    rd <- randomRegionOnGenome(10000) %>%
        runOverlappedRandomRegion(inputBed = test_bed)



    expect_equal(stepType(rd),"OverlappedRandomRegion")

    stepID(rd)

    expect_true(isReady(rd))

})
