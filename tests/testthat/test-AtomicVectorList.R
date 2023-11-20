# Test staging and loading of AtomicVectorLists.
# library(testthat); library(alabaster.ranges); source("setup.R"); source("test-AtomicVectorList.R")

test_that("staging and loading of atomic vector lists work as expected", {
    tmp <- tempfile()
    dir.create(tmp)

    X <- splitAsList(LETTERS, sample(3, 26, replace=TRUE))
    staged <- .validatedStage(X, tmp, path="test1")

    expect_true(file.exists(file.path(tmp, staged$path)))
    expect_true(file.exists(file.path(tmp, staged$atomic_vector_list$concatenated$resource$path)))
    expect_identical(staged$compressed_list$compression, "gzip")

    Y <- loadAtomicVectorList(staged, project=tmp)
    expect_identical(X, Y)

    # Try with and without names.
    X2 <- splitAsList(setNames(LETTERS, letters), sample(3, 26, replace=TRUE))
    staged <- .validatedStage(X2, tmp, path="test2")
    Y2 <- loadAtomicVectorList(staged, project=tmp)
    expect_identical(X2, Y2)

    # Try with and without names.
    X3 <- X
    names(X3) <- NULL
    staged <- .validatedStage(X3, tmp, path="test3")
    Y3 <- loadAtomicVectorList(staged, project=tmp)
    expect_identical(X3, Y3)
})

test_that("staging and loading of atomic vector lists work in the new world", {
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
