# Test stageGRanges on various bits and pieces.
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

test_that("stageObject works correctly", {
    tmp <- tempfile()
    dir.create(tmp)

    grlmeta <- .validatedStage(grl, tmp, "grl")
    expect_false(is.null(grlmeta$compressed_list$names))
    .writeMetadata(grlmeta, tmp)

    expect_null(grlmeta$compressed_list$element_data) # no metadata yet
    expect_null(grlmeta$compressed_list$other_data) 
    expect_identical(grlmeta$compressed_list$compression, "gzip")

    # Coordinate information is saved correctly. 
    grmeta <- jsonlite::fromJSON(file.path(tmp, paste0(grlmeta$genomic_ranges_list$concatenated$resource$path, ".json")), simplifyVector=FALSE)
    expect_false(is.null(grmeta$genomic_ranges$names))

    grouping <- read.csv(file.path(tmp, grlmeta$path), row.names=1)
    expect_false(is.null(grouping$number))
    expect_identical(rownames(grouping), names(grl))

    # Round trip works.
    grl2 <- loadGRangesList(grlmeta, tmp)
    expect_identical(grl2, grl)
})

test_that("stageObject handles its own metadata", {
    mcols(grl)$stuff <- sample(LETTERS, length(grl), replace=TRUE)

    tmp <- tempfile()
    dir.create(tmp)
    grlmeta <- .validatedStage(grl, tmp, "grl")

    emeta <- jsonlite::fromJSON(file.path(tmp, paste0(grlmeta$compressed_list$element_data$resource$path, ".json")), simplifyVector=FALSE)
    expect_identical(length(emeta$data_frame$columns), 1L)

    grl2 <- loadGRangesList(grlmeta, tmp)
    expect_identical(grl2, grl)
})

test_that("stageObject handles GRLs with internal metadata", {
    tmp <- tempfile()
    dir.create(tmp)

    mcols(grl@unlistData)$stuff <- rpois(length(grl@unlistData), lambda=5)
    grlmeta <- .validatedStage(grl, tmp, "grl")

    # Coordinate information is saved correctly. 
    cat.path <- grlmeta$genomic_ranges_list$concatenated$resource$path
    grmeta <- jsonlite::fromJSON(file.path(tmp, paste0(cat.path, ".json")), simplifyVector=FALSE)
    grdf <- read.csv(file.path(tmp, grmeta$genomic_ranges$range_data$resource$path))
    expect_identical(grdf$stuff, unlist(grl)$stuff)

    # Round trip works correctly.
    grl2 <- loadGRangesList(grlmeta, tmp)
    expect_identical(grl2, grl)
})

test_that("stageObject handles unnamed GRLs", {
    tmp <- tempfile()
    dir.create(tmp)

    names(grl@unlistData) <- NULL
    names(grl) <- NULL

    grlmeta <- .validatedStage(grl, tmp, "grl")
    expect_true(is.null(grlmeta$compressed_list$names))
    grmeta <- jsonlite::fromJSON(file.path(tmp, paste0(grlmeta$genomic_ranges_list$concatenated$resource$path, ".json")), simplifyVector=FALSE)
    expect_true(is.null(grmeta$compressed_list$names))

    # Round trip works.
    grl2 <- loadGRangesList(grlmeta, tmp)
    expect_identical(grl2, grl)
})

test_that("stageObject handles empty GRLs", {
    tmp <- tempfile()
    dir.create(tmp)

    copy <- GRangesList(rep(list(GRanges()), 100))
    names(copy) <- seq_len(100)

    grlmeta <- .validatedStage(copy, tmp, "grl")

    grl2 <- loadGRangesList(grlmeta, tmp)
    expect_identical(grl2, copy)
})

test_that("stageObject works with extra metadata", {
    metadata(grl) <- list(WHEE="foo")

    tmp <- tempfile()
    dir.create(tmp)
    out <- .validatedStage(grl, tmp, "thing")
    expect_false(is.null(out$compressed_list$other_data))

    grl2 <- loadGRangesList(out, tmp)
    expect_equal(grl, grl2)
})
