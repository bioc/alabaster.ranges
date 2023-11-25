#' Save compressed list of atomic vectors to disk
#'
#' Save a \linkS4class{CompressedAtomicList} object to its on-disk representation.
#'
#' @param x A \linkS4class{CompressedAtomicList} object.
#' @inheritParams alabaster.base::saveObject
#'
#' @return 
#' \code{x} is saved to \code{path}, and \code{NULL} is invisibly returned.
#'
#' @seealso
#' \code{\link{readAtomicVectorList}}, to read a \linkS4class{CompressedAtomicList} from disk.
#'
#' @author Aaron Lun
#' @examples
#' library(S4Vectors)
#' X <- splitAsList(LETTERS, sample(3, 26, replace=TRUE))
#'
#' tmp <- tempfile()
#' saveObject(X, tmp)
#' list.files(tmp, recursive=TRUE)
#'
#' @export
#' @aliases stageObject,CompressedAtomicList-method
#' @rdname saveAtomicVectorList
setMethod("saveObject", "CompressedAtomicList", function(x, path, ...) .save_compressed_list(x, path=path, name="atomic_vector_list", ...))

#' @import BiocGenerics IRanges rhdf5 alabaster.base
.save_compressed_list <- function(x, path, name, ...) {
    dir.create(path, showWarnings=FALSE)
    saveObject(unlist(x, use.names=FALSE), file.path(path, "concatenated"), ...)
    saveMetadata(
         x,
         metadata.path=file.path(path, "other_annotations"),
         mcols.path=file.path(path, "element_annotations"),
         ...
    )

    fpath <- file.path(path, "partitions.h5")
    fhandle <- H5Fcreate(fpath, "H5F_ACC_TRUNC")
    on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)

    ghandle <- H5Gcreate(fhandle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)
    h5_write_attribute(ghandle, "version", "1.0", scalar=TRUE)

    h5_write_vector(ghandle, "lengths", lengths(x), type="H5T_NATIVE_UINT32")
    if (!is.null(names(x))) {
        h5_write_vector(ghandle, "names", names(x))
    }

    write(file=file.path(path, "OBJECT"), name)
    invisible(NULL)
}

##############################
######### OLD STUFF ##########
##############################

#' @export
#' @import S4Vectors
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

    # Automatically returns NULL if it's the names are NULL.
    element_data <- .processMcols(x, dir, path, mcols.name)
    other_data <- .processMetadata(x, dir, path, meta.name)

    meta <- list(
        `$schema`=schema,
        path=path2, 
        is_child=child,
        compressed_list=list(
            length=length(x),
            names=!is.null(names(x)),
            element_data=element_data,
            other_data=other_data,
            compression="gzip"
        )
    )

    meta[[dirname(schema)]] <- list(concatenated=list(resource=cat.info))

    meta
}
