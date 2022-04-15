test_that("general", {
    expect_error(readBrukerFlexFile("./"))
    expect_error(readBrukerFlexFile("test_readBrukerFlexFile.R"))
    expect_error(readBrukerFlexFile("ape"))
})
