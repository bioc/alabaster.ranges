#' Stage a compressed list of atomic vectors
#'
#' Stage a \linkS4class{CompressedAtomicList} object.
#'
#' @inheritParams alabaster.base::stageObject
#' @param group.name String containing the name of the file inside \code{path} to save the list groupings.
#' @param concat.name String containing the extension-less name of the file inside \code{path} to save the concatenated elements.
#' @param mcols.name String specifying the name of the directory inside \code{path} to save the \code{\link{mcols}}.
#' If \code{NULL}, per-element metadata is not saved.
#' @param meta.name String specifying the name of the directory inside \code{path} to save \code{\link{metadata}(x)}.
#' If \code{NULL}, object metadata is not saved.
#'
#' @return 
#' A named list containing the metadata for \code{x}.
#' The contents of \code{x} are saved inside \code{path} and referenced from the metadata. 
#'
#' @details
#' The staging process will save both the interval groupings and the concatenated vector into separate files.
#' The concatenated vector is coerced into a DataFrame and staged with the corresponding \code{\link{stageObject}} method.
#' The CompressedList may also be decorated with metadata for each list element in its \code{\link{mcols}}, which is saved to \code{mcols.name}.
#' No file is created at \code{mcols.name} if \code{\link{mcols}(x)} is \code{NULL} or has no columns.
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile()
#' dir.create(tmp)
#' 
#' library(S4Vectors)
#' X <- splitAsList(LETTERS, sample(3, 26, replace=TRUE))
#' stageObject(X, tmp, path="test1")
#' list.files(file.path(tmp, "test1"))
#'
#' @export
#' @rdname stageAtomicVectorList
#' @importFrom BiocGenerics unlist
#' @importFrom S4Vectors DataFrame
#' @importClassesFrom IRanges CompressedAtomicList
setMethod("stageObject", "CompressedAtomicList", function(x, dir, path, child=FALSE, group.name="grouping", concat.name="concatenated", mcols.name="mcols", meta.name="other") {
    rd <- DataFrame(values=unlist(x, use.names=FALSE))
    .compressed_stager(x, 
        concatenated=rd, 
        schema="atomic_vector_list/v1.json",
        dir=dir, 
        path=path, 
        child=child,
        group.name=group.name, 
        concat.name=concat.name, 
        mcols.name=mcols.name,
        meta.name=meta.name
    )
})

#' @import alabaster.base
.compressed_stager <- function(x, concatenated, schema, dir, path, group.name, concat.name, mcols.name, meta.name, extra.args=list(), child=FALSE) {
    dir.create(file.path(dir, path))

    cat.args <- c(list(concatenated, dir, file.path(path, concat.name), child=TRUE), extra.args)
    cat.info <- tryCatch({ 
        info <- do.call(.stageObject, cat.args)
        .writeMetadata(info, dir)
    }, error=function(e) {
        stop("failed to stage 'unlist(<", class(x)[1], ">)'\n  - ", e$message)
    })

    path2 <- file.path(path, paste0(group.name, ".csv.gz"))
    ofile <- file.path(dir, path2)
    rd <- data.frame(number=lengths(x))
    if (!is.null(names(x))){ 
        rd <- cbind(row_names=names(x), rd)
    }
    .quickWriteCsv(rd, path=ofile)

    element_data <- NULL    
    if (!is.null(mcols.name)) {
        element_data <- .processMcols(x, dir, path, mcols.name)
    }

    other_data <- NULL
    if (!is.null(meta.name)) {
        other_data <- .processMetadata(x, dir, path, meta.name)
    }

    meta <- list(
        `$schema`=schema,
        path=path2, 
        is_child=child,
        compressed_list=list(
            length=length(x),
            names=.uniqueValues(names(x)),
            element_data=element_data,
            other_data=other_data,
            compression="gzip"
        )
    )

    meta[[dirname(schema)]] <- list(concatenated=list(resource=cat.info))

    meta
}
