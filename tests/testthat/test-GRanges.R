# library(testthat); library(alabaster.ranges); source("setup.R"); source("test-GRanges.R")

set.seed(1000)
gr <- GRanges(
    paste0('chr', sample(LETTERS[1:3], 100, replace=TRUE)),
    IRanges(sample(1000, 100), width=sample(100, 100, replace=TRUE)),
    strand=sample(c("+", "-", "*"), 100, replace=TRUE)
)
names(gr) <- paste0("Gene_", seq_along(gr))

test_that("saving GRs works correctly", {
    tmp <- tempfile()
    saveObject(gr, tmp)
    gr2 <- readObject(tmp)
    expect_identical(gr, gr2)
})

test_that("saving GRs handles non-trivial seqinfo", {
    seqlengths(gr) <- setNames(1000 * (1 + runif(length(seqlengths(gr)))), seqlevels(gr))

    tmp <- tempfile()
    saveObject(gr, tmp)
    gr2 <- readObject(tmp)
    expect_identical(gr, gr2)
})

test_that("saving GRs works without names", {
    names(gr) <- NULL

    tmp <- tempfile()
    saveObject(gr, tmp)
    gr2 <- readObject(tmp)
    expect_identical(gr, gr2)
})

test_that("saving GRs works with extra mcols", {
    gr$stuff <- runif(length(gr))
    gr$foo <- sample(LETTERS, length(gr), replace=TRUE)

    ntmp <- tempfile()
    saveObject(gr, ntmp)
    gr2 <- readObject(ntmp)
    expect_identical(gr, gr2)
})

test_that("saving GRs works with extra metadata", {
    metadata(gr) <- list(WHEE="foo")

    ntmp <- tempfile()
    saveObject(gr, ntmp)
    gr2 <- readObject(ntmp)
    expect_identical(gr, gr2)
})
