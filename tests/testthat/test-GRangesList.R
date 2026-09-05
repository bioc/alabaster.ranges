# library(testthat); library(alabaster.ranges); source("setup.R"); source("test-GRangesList.R")

set.seed(1000)
gr <- GRanges(
    paste0('chr', sample(LETTERS[1:3], 100, replace=TRUE)),
    IRanges(sample(1000, 100), width=sample(100, 100, replace=TRUE)),
    strand=sample(c("+", "-", "*"), 100, replace=TRUE)
)
names(gr) <- paste0("Exon_", seq_along(gr))

grl <- splitAsList(gr, sample(length(gr), 100, replace=TRUE))
names(grl) <- paste0("Gene_", seq_along(grl))

test_that("saving GRLs works correctly", {
    tmp <- tempfile()
    saveObject(grl, tmp)
    expect_identical(grl, readObject(tmp))
})

test_that("saving GRLs handles its own metadata", {
    mcols(grl)$stuff <- sample(LETTERS, length(grl), replace=TRUE)

    tmp <- tempfile()
    saveObject(grl, tmp)
    expect_identical(grl, readObject(tmp))
})

test_that("saving GRLs handles GRLs with internal metadata", {
    mcols(grl@unlistData)$stuff <- rpois(length(grl@unlistData), lambda=5)

    tmp <- tempfile()
    saveObject(grl, tmp)
    expect_identical(grl, readObject(tmp))
})

test_that("saving GRLs handles unnamed GRLs", {
    names(grl@unlistData) <- NULL
    names(grl) <- NULL

    tmp <- tempfile()
    saveObject(grl, tmp)
    expect_identical(grl, readObject(tmp))
})

test_that("saving GRLs handles empty GRLs", {
    copy <- GRangesList(rep(list(GRanges()), 100))
    names(copy) <- seq_len(100)

    tmp <- tempfile()
    saveObject(copy, tmp)
    expect_identical(copy, readObject(tmp))
})

test_that("saving GRLs works with extra metadata", {
    metadata(grl) <- list(WHEE="foo")

    tmp <- tempfile()
    saveObject(grl, tmp)
    expect_identical(grl, readObject(tmp))
})
