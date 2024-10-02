test_that("sampleName", {
    files <- c(
        "./Run1/2-100kDa/0_A1/1/1SLin/fid",
        "foo/Pankreas_HB_L_061019_A10/0_a19/1/1SLin/fid",
        "LT_1631_05_smear/0_H1_1SLin/fid"
    )
    name <- c("2_100kDa", "Pankreas_HB_L_061019_A10", "LT_1631_05_smear")

    for (i in seq_along(files))
        expect_equal(.sampleName(files[i]), name[i])
})
