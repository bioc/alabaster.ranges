#' Save a Seqinfo object to disk
#'
#' Save a \linkS4class{Seqinfo} object to its on-disk representation.
#'
#' @param x A \linkS4class{Seqinfo} object. 
#' @inheritParams alabaster.base::saveObject
#'
#' @return 
#' \code{x} is saved to \code{path}, and \code{NULL} is invisibly returned.
#'
#' @seealso
#' \code{\link{readSeqinfo}}, to read a \linkS4class{Seqinfo} from disk.
#'
#' @examples
#' si <- Seqinfo(c("chrA", "chrB"), c(1000, 2000))
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' saveObject(si, tmp, path="seqinfo")
#' list.files(tmp, recursive=TRUE)
#'
#' @export
#' @aliases stageObject,Seqinfo-method
#' @rdname saveSeqinfo
#' @import rhdf5 alabaster.base GenomeInfoDb
setMethod("saveObject", "Seqinfo", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)
    fpath <- file.path(path, "info.h5")
    fhandle <- H5Fcreate(fpath, "H5F_ACC_TRUNC")
    on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)

    name <- "sequence_information"
    ghandle <- H5Gcreate(fhandle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)
    h5_write_attribute(ghandle, "version", "1.0", scalar=TRUE)

    sq <- seqnames(x)
    if (anyNA(sq)) {
        stop("'seqnames(<", class(x)[1], ">)' should not be missing")
    }
    h5_write_vector(ghandle, "name", sq)

    trans <- transformVectorForHdf5(genome(x))
    local({
        dhandle <- h5_write_vector(ghandle, "genome", trans$transformed, emit=TRUE)
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        if (!is.null(trans$placeholder)) {
            h5_write_attribute(dhandle, missingPlaceholderName, trans$placeholder, scalar=TRUE)
        }
    })

    trans <- transformVectorForHdf5(isCircular(x))
    local({
        dhandle <- h5_write_vector(ghandle, "circular", trans$transformed, emit=TRUE)
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        if (!is.null(trans$placeholder)) {
            h5_write_attribute(dhandle, missingPlaceholderName, trans$placeholder, scalar=TRUE)
        }
    })

    ll <- seqlengths(x)
    placeholder <- NULL
    if (anyNA(ll)) {
        placeholder <- 2^32 - 1
        ll <- as.double(ll)
        ll[is.na(ll)] <- placeholder
    }
    local({
        dhandle <- h5_write_vector(ghandle, "length", ll, type="H5T_NATIVE_UINT32", emit=TRUE)
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        if (!is.null(placeholder)) {
            h5_write_attribute(dhandle, missingPlaceholderName, placeholder, type="H5T_NATIVE_UINT32", scalar=TRUE)
        }
    })

    write(file=file.path(path, "OBJECT"), name)
    invisible(NULL)
})

##############################
######### OLD STUFF ##########
##############################

#' @export
setMethod("stageObject", "Seqinfo", function(x, dir, path, child=FALSE) {
    dir.create(file.path(dir, path), showWarnings=FALSE)

    si.path <- file.path(path, "simple.csv.gz")
    x <- as.data.frame(x)
    si <- cbind(seqnames=rownames(x), x)
    .quickWriteCsv(si, path=file.path(dir, si.path))

    list(
        `$schema`="sequence_information/v1.json",
        path=si.path,
        is_child=child,
        sequence_information=list(
            dimensions=dim(x),
            compression="gzip"
        )
    )
})
