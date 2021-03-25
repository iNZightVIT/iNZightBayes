test_that("Estimate mean returns the correct type of value", {
    expect_s3_class(estimate_mean(1:10), "inzposterior")
})
