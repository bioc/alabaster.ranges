# library(testthat); library(alabaster.ranges); source('setup.R'); source("test-Seqinfo.R")

SI <- Seqinfo(c("chrA", "chrB", "chrC"), c(1000, 20000, 300))

test_that("saving Seqinfo works correctly", {
    SI <- Seqinfo(c("chrA", "chrB", "chrC"), c(1000, 20000, 300))
    tmp <- tempfile()
    out <- saveObject(SI, tmp)
    si2 <- readObject(tmp)
    expect_identical(SI, si2)

    # Missing sequence lengths. 
    SI <- Seqinfo(c("chrA", "chrB", "chrC"))
    tmp <- tempfile()
    out <- saveObject(SI, tmp)
    si2 <- readObject(tmp)
    expect_identical(SI, si2)

    # Adding all details.
    SI <- Seqinfo(c("chrA", "chrB", "chrC"), c(99, 999, 9999), genome=c("mm10", "hg38", "z10"), isCircular=c(TRUE, FALSE, FALSE))
    tmp <- tempfile()
    out <- saveObject(SI, tmp)
    si2 <- readObject(tmp)
    expect_identical(SI, si2)
})
