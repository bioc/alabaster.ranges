# Test stageObject on various bits and pieces.
# library(testthat); library(alabaster.ranges); source('setup.R'); source("test-Seqinfo.R")

SI <- Seqinfo(c("chrA", "chrB", "chrC"), c(1000, 20000, 300))

test_that("stageObject works correctly", {
    tmp <- tempfile()
    dir.create(tmp)
    out <- stageObject(SI, tmp, "thing")

    rr <- read.csv(file.path(tmp, out$path), row.names=1)
    expect_identical(rownames(rr), seqnames(SI))
    expect_identical(rr$seqlengths, unname(seqlengths(SI)))

    # Metadata is saved properly.
    expect_false(is.null(out$sequence_information))
    expect_identical(out$sequence_information$compression, "gzip")

    # Round trip works.
    si2 <- loadSeqinfo(out, tmp)
    expect_identical(SI, si2)
})

test_that("stageObject works correctly in the new world", {
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
