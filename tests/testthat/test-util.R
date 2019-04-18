context("test some util functions")


test_that("test genome configuration ",{

    expect_equal(getBasenamePrefix("aaa/bbb.ccc.ddd","cCc"),"bbb")

    expect_equal(getBasenamePrefix("aaa/bbb.ccc.ddd","ddd"),"bbb.ccc")

    expect_equal(getPathPrefix("aaa/bbb.ccc.ddd","dDd"),"aaa/bbb.ccc")

    expect_equal(getPathPrefix("aaa/bbb.ccc.ddd","ccc"),"aaa/bbb")

    file.create("test.bed")

    expect_silent(checkFileExist("test.bed"))

    expect_error(checkFileExist("test.bed1"))

    dir.create("testdir")

    expect_silent(checkPathExist(file.path(getwd(),"testdir")))

    expect_error(checkPathExist(file.path(dirname(getwd()),"notexistfolder","testdir")))

    expect_silent(checkFileCreatable("aaa.bed"))

    expect_warning(expect_error(checkFileCreatable("testdir1/aaa.bed")))


})
