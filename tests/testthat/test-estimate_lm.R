test_that("Estimate mean returns the correct type of value", {
    expect_silent(
        fit <- estimate_lm(Sepal.Width ~ Sepal.Length, data = iris)
    )
    expect_s3_class(fit, "inzposterior")
    expect_output(print(fit), "A posterior distribution")

    expect_s3_class(plot(fit), "gg")
    expect_s3_class(summary(fit), "summary.mcmc")
})
