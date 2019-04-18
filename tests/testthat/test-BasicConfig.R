context("test basic configuration")


test_that("test genome configuration ",{

    allvalidgenome <- c("hg19",
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
    expect_equivalent(getValidGenome(),allvalidgenome)

    setGenome("hg19")

    expect_equal(getGenome(),"hg19")

})




test_that("test thread configuration ",{

    setThreads()


    expect_equal( getThreads(),detectCores())

})



test_that("test reference configuration ",{

    setRefDir("./refdir")

    expect_equal( normalizePath(getRefDir()),normalizePath("./refdir"))
    expect_equal(getRef("test")$file,"fileName")
    expect_equal(getRef("test")$rc,"obj")

    expect_equal(getRefFiles("test"),"fileName")
    expect_equal(getRefRc("test"),"obj")

})




test_that("test temporary directory configuration ",{

    setTmpDir()

    expect_equal( getTmpDir(),getwd())


})


test_that("test job directory configuration ",{

    setJobName("testJobName")
    expect_equal(getJobName(),"testJobName")
    expect_equal(getJobDir(),file.path(getTmpDir(),getJobName()))


})



test_that("test run with finish check configuration ",{


    checkAndInstall <- function(){
        runWithFinishCheck(func = checkAndInstallBSgenome,refName = "bsgenome")
        runWithFinishCheck(func = checkAndInstallGenomeFa,refName = "fasta",
                           refFilePath = paste0(getGenome(),".fa"))
    }
    initPipeFrame(availableGenome = c("hg19", "hg38","mm9","mm10","testgenome"),
                  defaultJobName = paste0("pkgname","-pipeline")
    )

    expect_equivalent(getValidGenome(), c("hg19", "hg38","mm9","mm10","testgenome"))

})
