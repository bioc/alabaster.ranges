# library(testthat); library(alabaster.ranges); source("setup.R"); source("test-DataFrameList.R")

test_that("saving of data frame lists works correctly", {
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
