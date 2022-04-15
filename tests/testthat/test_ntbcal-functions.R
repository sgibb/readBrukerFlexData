test_that(".extractV10CTOF2CalibrationConstants", {
    x <- "##$NTBCal= <V3.0CCalibrator 12 1 0 0 -2147483648 2147483647  V1.0CHPCData  Order 0 vCoeff V1.0VectorDouble 0  c2 0 c0 0 minMass 0 maxMass 0 bUse 0 endCHPCData V1.0CTOF2CalibrationConstants 48 0.25 3884.868239229128 1164156.9998801197 6.1682474689786355 -0.063083671657452864 -34.362308485323794 2 V1.0CTOF2CalibrationConstants 0 0 0 0 0 0 0 -1  >"
    r <- c(48, 0.25, 3884.868239229128, 1164156.9998801197, 6.1682474689786355, -0.063083671657452864, -34.362308485323794, 2)
    expect_identical(.extractV10CTOF2CalibrationConstants(x), r)
})
