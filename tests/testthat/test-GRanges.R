# Test stageObject on various bits and pieces.
# library(testthat); library(alabaster.ranges); source("setup.R"); source("test-GRanges.R")

set.seed(1000)
gr <- GRanges(
    paste0('chr', sample(LETTERS[1:3], 100, replace=TRUE)),
    IRanges(sample(1000, 100), width=sample(100, 100, replace=TRUE)),
    strand=sample(c("+", "-", "*"), 100, replace=TRUE)
)
names(gr) <- paste0("Gene_", seq_along(gr))

test_that("stageObject works correctly", {
    tmp <- tempfile()
    dir.create(tmp)
    out <- stageObject(gr, tmp, "thing")

    rr <- read.csv(file.path(tmp, out$path), row.names=1)
    expect_identical(rownames(rr), names(gr))
    expect_identical(rr$seqnames, as.character(seqnames(gr)))
    expect_identical(rr$start, start(gr))
    expect_identical(rr$end, end(gr))

    # Metadata is saved properly.
    expect_true(out$genomic_ranges$names)
    expect_null(out$genomic_ranges$range_data) # no mcols yet, see below.
    expect_null(out$genomic_ranges$other_data) # no metadata yet, see below.
    expect_identical(out$genomic_ranges$compression, "gzip")

    # Round trip works.
    gr2 <- loadGRanges(out, tmp)
    expect_identical(gr, gr2)
})

test_that("stageObject handles non-trivial seqinfo", {
    seqlengths(gr) <- setNames(1000 * (1 + runif(length(seqlengths(gr)))), seqlevels(gr))

    tmp <- tempfile()
    dir.create(tmp)
    out <- stageObject(gr, tmp, "thing")

    # Sequencing information is saved properly.
    si <- read.csv(file.path(tmp, out$genomic_ranges$sequence_information$resource$path), row.names=1)
    expect_identical(setNames(si$seqlengths, rownames(si)), seqlengths(gr))

    # Round trip works.
    gr2 <- loadGRanges(out, tmp)
    expect_identical(gr, gr2)
})

test_that("stageObject works without names", {
    names(gr) <- NULL

    tmp <- tempfile()
    dir.create(tmp)
    out <- stageObject(gr, tmp, "thing")
    expect_false(out$genomic_ranges$names)

    gr2 <- loadGRanges(out, tmp)
    expect_identical(gr, gr2)
})

test_that("stageObject works with extra mcols", {
    gr$stuff <- runif(length(gr))
    gr$foo <- sample(LETTERS, length(gr), replace=TRUE)

    tmp <- tempfile()
    dir.create(tmp)
    out <- stageObject(gr, tmp, "thing")
    expect_false(is.null(out$genomic_ranges$range_data))

    # Checking that they aren't present in the CSV.
    rr <- read.csv(file.path(tmp, out$path), row.names=1)
    expect_null(rr$stuff)
    expect_null(rr$foo)

    gr2 <- loadGRanges(out, tmp)
    expect_equal(gr, gr2)
})

test_that("stageObject works with extra metadata", {
    metadata(gr) <- list(WHEE="foo")

    tmp <- tempfile()
    dir.create(tmp)
    out <- stageObject(gr, tmp, "thing")
    expect_false(is.null(out$genomic_ranges$other_data))

    gr2 <- loadGRanges(out, tmp)
    expect_equal(gr, gr2)
})

