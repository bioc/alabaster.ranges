#' Read a Seqinfo from disk
#'
#' Read a \linkS4class{Seqinfo} object from its on-disk representation.
#' 
#' @param path String containing a path to a directory, itself created with the \code{\link{saveObject}} method for Seqinfo objects.
#' @param ... Further arguments, ignored.
#'
#' @return A \linkS4class{Seqinfo} object.
#' @seealso
#' \code{"\link{saveObject,Seqinfo-method}"} for the corresponding saving method.
#'
#' @examples
#' si <- Seqinfo(c("chrA", "chrB"), c(1000, 2000))
#'
#' tmp <- tempfile()
#' saveObject(si, tmp)
#' readSeqinfo(tmp)
#' 
#' @export
#' @aliases loadSeqinfo
#' @import rhdf5 alabaster.base
readSeqinfo <- function(path, ...) {
    fpath <- file.path(path, "info.h5")

    fhandle <- H5Fopen(fpath, "H5F_ACC_RDONLY")
    on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)

    name <- "sequence_information"
    ghandle <- H5Gopen(fhandle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    seqnames <- h5_read_vector(ghandle, "name")

    genome <- local({
        dhandle <- H5Dopen(ghandle, "genome")
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        contents <- H5Dread(dhandle, drop=TRUE)
        missing.placeholder <- h5_read_attribute(dhandle, missingPlaceholderName, check=TRUE, default=NULL)
        h5_cast(contents, expected.type="string", missing.placeholder=missing.placeholder)
    })

    circular <- local({ 
        dhandle <- H5Dopen(ghandle, "circular")
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        contents <- H5Dread(dhandle, drop=FALSE)
        missing.placeholder <- h5_read_attribute(dhandle, missingPlaceholderName, check=TRUE, default=NULL)
        h5_cast(contents, expected.type="boolean", missing.placeholder=missing.placeholder)
    })

    seqlengths <- local({
        dhandle <- H5Dopen(ghandle, "length")
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        contents <- H5Dread(dhandle, bit64conversion="double")
        missing.placeholder <- h5_read_attribute(dhandle, missingPlaceholderName, check=TRUE, default=NULL, bit64conversion="double")
        h5_cast(contents, expected.type="integer", missing.placeholder=missing.placeholder)
    })

    Seqinfo(seqnames=seqnames, seqlengths=seqlengths, isCircular=circular, genome=genome)
}

##############################
######### OLD STUFF ##########
##############################

#' @export
loadSeqinfo <- function(info, project) {
    si.path <- acquireFile(project, info$path)
    si.df <- .quickReadCsv(si.path, 
        c(seqnames="character", seqlengths="integer", isCircular="logical", genome="character"), 
        row.names=FALSE,
        compression=info$sequence_information$compression, 
        expected.nrows=info$sequence_information$dimensions[[1]]
    )
    Seqinfo(
        seqnames=as.character(si.df$seqnames), 
        seqlengths=as.integer(si.df$seqlengths), 
        isCircular=as.logical(si.df$isCircular), 
        genome=as.character(si.df$genome)
    )
}
