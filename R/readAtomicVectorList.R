#' Load an atomic vector list
#'
#' Load a list of atomic vectors as a \linkS4class{CompressedAtomicList} from its on-disk representation.
#' This is usually not directly called by users, but is instead called by dispatch in \code{\link{readObject}}.
#'
#' @param path String containing a path to a directory, itself created with the \code{\link{saveObject}} method for \linkS4class{CompressedAtomicList}s.
#' @param metadata Named list of metadata for this object, see \code{\link{readObjectFile}} for details.
#' @param ... Further arguments, to be passed to internal \code{\link{altReadObject}} calls.
#'
#' @return A CompressedAtomicList of the relevant type.
#'
#' @seealso
#' \code{"\link{saveObject,CompressedAtomicList-method}"}, to save an object to disk.
#'
#' @author Aaron Lun
#'
#' @examples
#' library(S4Vectors)
#' X <- splitAsList(LETTERS, sample(3, 26, replace=TRUE))
#'
#' tmp <- tempfile()
#' saveObject(X, tmp)
#' readObject(tmp)
#'
#' @export
#' @aliases loadAtomicVectorList
readAtomicVectorList <- function(path, metadata, ...) {
    .read_compressed_list(path, metadata, "atomic_vector_list", ...)
}

#' @import BiocGenerics IRanges rhdf5 alabaster.base
.read_compressed_list <- function(path, metadata, name, ...) {
    concat <- altReadObject(file.path(path, "concatenated"), ...)

    fpath <- file.path(path, "partitions.h5")
    fhandle <- H5Fopen(fpath)
    on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)
    ghandle <- H5Gopen(fhandle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    runs <- h5_read_vector(ghandle, "lengths")
    output <- relist(concat, PartitioningByWidth(x=runs))
    if (h5_object_exists(ghandle, "names")) {
        names(output) <- h5_read_vector(ghandle, "names")
    }

    readMetadata(
        output, 
        mcols.path=file.path(path, "element_annotations"),
        metadata.path=file.path(path, "other_annotations"),
        ...
    )
}

##############################
######### OLD STUFF ##########
##############################

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
