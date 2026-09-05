# library(testthat); library(alabaster.ranges); source("setup.R"); source("test-AtomicVectorList.R")

test_that("saving of atomic vector lists works correctly", {
    X <- splitAsList(LETTERS, sample(3, 26, replace=TRUE))

    tmp <- tempfile()
    saveObject(X, tmp)
    Y <- readObject(tmp)
    expect_identical(X, Y)

    # Try with inner names.
    X2 <- splitAsList(setNames(LETTERS, letters), sample(3, 26, replace=TRUE))
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
