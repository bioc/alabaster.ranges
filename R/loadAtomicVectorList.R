#' Load an atomic vector list
#'
#' Load a list of atomic vectors as a \linkS4class{CompressedAtomicList}, typically from files created by the corresponding \code{\link{stageObject}} method.
#'
#' @inheritParams alabaster.base::loadDataFrame
#'
#' @return A CompressedAtomicList of the relevant type.
#'
#' @author Aaron Lun
#'
#' @examples
#' library(S4Vectors)
#' X <- splitAsList(LETTERS, sample(3, 26, replace=TRUE))
#'
#' # Staging this object:
#' tmp <- tempfile()
#' dir.create(tmp)
#' info <- stageObject(X, tmp, path="test1")
#'
#' # Loading the object:
#' loadAtomicVectorList(info, tmp)
#'
#' @export
loadAtomicVectorList <- function(info, project) {
    concat.info <- acquireMetadata(project, info$atomic_vector_list$concatenated$resource$path)
    concat <- .loadObject(concat.info, project=project)
    unlisted <- concat[,1]
    names(unlisted) <- rownames(concat)
    .load_compressed(unlisted, info, project)
}

#' @importFrom BiocGenerics relist
#' @importFrom IRanges PartitioningByWidth
.load_compressed <- function(contents, info, project) {
    path <- acquireFile(project, info$path)

    has.names <- isTRUE(info$compressed_list$names)
    groups <- .quickReadCsv(path, 
        c(number="integer"), 
        row.names=has.names,
        compression=info$compressed_list$compression,
        expected.nrows=info$compressed_list$length
    )

    runs <- groups[,1]
    if (has.names) {
        names(runs) <- rownames(groups)
    }

    output <- relist(contents, PartitioningByWidth(x=runs))
    .restoreMetadata(output, mcol.data=info$compressed_list$element_data, meta.data=info$compressed_list$other_data, project=project)
}
