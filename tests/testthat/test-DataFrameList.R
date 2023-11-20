# Test stageObjects on various bits and pieces.
# library(testthat); library(alabaster.ranges); source("setup.R"); source("test-DataFrameList.R")

test_that("staging and loading of data frame lists work as expected", {
    tmp <- tempfile()
    dir.create(tmp)

    df <- DataFrame(alpha=letters, bravo=26:1)
    X <- splitAsList(df, sample(paste0("G", 1:3), 26, replace=TRUE))
    staged <- .validatedStage(X, tmp, path="test1")

    expect_true(file.exists(file.path(tmp, staged$path)))
    expect_true(file.exists(file.path(tmp, staged$data_frame_list$concatenated$resource$path)))
    expect_identical(staged$compressed_list$compression, "gzip")

    Y <- loadDataFrameList(staged, project=tmp)
    expect_identical(X, Y)

    # Try with and without names.
    rownames(df) <- paste0("ROW", 1:nrow(df))
    X2 <- splitAsList(df, sample(3, 26, replace=TRUE))
    staged <- .validatedStage(X2, tmp, path="test2")
    Y2 <- loadDataFrameList(staged, project=tmp)
    expect_identical(X2, Y2)

    # Try with and without names.
    X3 <- X
    names(X3) <- NULL
    staged <- .validatedStage(X3, tmp, path="test3")
    Y3 <- loadDataFrameList(staged, project=tmp)
    expect_identical(X3, Y3)
})

test_that("staging and loading of data frame lists work in the new world", {
    df <- DataFrame(alpha=letters, bravo=26:1)
    X <- splitAsList(df, sample(paste0("G", 1:3), 26, replace=TRUE))

    tmp <- tempfile()
    saveObject(X, tmp)
    Y <- readObject(tmp)
    expect_identical(X, Y)

    # Try with inner names.
    rownames(df) <- paste0("ROW", 1:nrow(df))
    X2 <- splitAsList(df, sample(3, 26, replace=TRUE))

    tmp <- tempfile()
    saveObject(X2, tmp)
    Y <- readObject(tmp)
    expect_identical(X2, Y)

    # Try without outer names.
    X3 <- X
    names(X3) <- NULL

    tmp <- tempfile()
    saveObject(X3, tmp)
    Y <- readObject(tmp)
    expect_identical(X3, Y)

    # Slapping on some metadata.
    X4 <- X
    metadata(X4) <- list(WHEE="foo")
    mcols(X4)$info <- runif(length(X))

    tmp <- tempfile()
    saveObject(X4, tmp)
    Y <- readObject(tmp)
    expect_identical(X4, Y)
})
