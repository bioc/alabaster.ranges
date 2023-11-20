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

    name <- "sequence_information"
    seqnames <- as.vector(h5read(fpath, paste0(name, "/name")))
    seqnames <- as.character(seqnames)

    gpath <- paste0(name, "/genome")
    genome <- as.vector(h5read(fpath, gpath))
    placeholder <- h5readAttributes(fpath, gpath)[["missing-value-placeholder"]]
    if (!is.null(placeholder)) {
        genome[genome == placeholder] <- NA
    }
    genome <- as.character(genome)

    cpath <- paste0(name, "/circular")
    circular <- as.vector(h5read(fpath, cpath))
    placeholder <- h5readAttributes(fpath, cpath)[["missing-value-placeholder"]]
    if (!is.null(placeholder)) {
        circular[circular == placeholder] <- NA
    }
    circular <- as.logical(circular)

    lpath <- paste0(name, "/length")
    seqlengths <- as.vector(h5read(fpath, lpath, bit64conversion="double"))
    placeholder <- h5readAttributes(fpath, lpath, bit64conversion="double")[["missing-value-placeholder"]]
    if (!is.null(placeholder)) {
        seqlengths[seqlengths == placeholder] <- NA
    }
    seqlengths <- as.integer(seqlengths)

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
