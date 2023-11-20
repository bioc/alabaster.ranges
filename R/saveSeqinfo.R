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

    h5createFile(fpath)
    name <- "sequence_information"
    h5createGroup(fpath, name)

    sq <- seqnames(x)
    if (anyNA(sq)) {
        stop("'seqnames(<", class(x)[1], ">)' should not be missing")
    }
    h5write(sq, fpath, paste0(name, "/name"))

    trans <- transformVectorForHdf5(genome(x))
    genome.path <- paste0(name, "/genome")
    h5write(trans$transformed, fpath, genome.path)
    if (!is.null(trans$placeholder)) {
        addMissingPlaceholderAttributeForHdf5(fpath, genome.path, trans$placeholder)
    }

    trans <- transformVectorForHdf5(isCircular(x))
    circ.path <- paste0(name, "/circular")
    h5write(trans$transformed, fpath, circ.path)
    if (!is.null(trans$placeholder)) {
        addMissingPlaceholderAttributeForHdf5(fpath, circ.path, trans$placeholder)
    }

    ll <- seqlengths(x)
    len.path <- paste0(name, "/length")
    h5createDataset(fpath, len.path, dims=length(ll), H5type = "H5T_NATIVE_UINT32")
    placeholder <- NULL
    if (anyNA(ll)) {
        placeholder <- 2^32 - 1
        ll <- as.double(ll)
        ll[is.na(ll)] <- placeholder
    }
    h5write(ll, fpath, len.path)
    if (!is.null(placeholder)) {
        (function() {
            fhandle <- H5Fopen(fpath)
            on.exit(H5Fclose(fhandle))
            dhandle <- H5Dopen(fhandle, len.path)
            on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
            ahandle <- H5Acreate(dhandle, "missing-value-placeholder", "H5T_NATIVE_UINT32", H5Screate("H5S_SCALAR"))
            on.exit(H5Aclose(ahandle), add=TRUE, after=FALSE)
            H5Awrite(ahandle, placeholder)
        })()
    }

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
