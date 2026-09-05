#' Read a GRangesList from disk
#'
#' Read a \link[GenomicRanges]{GRangesList} object from its on-disk representation.
#' 
#' @param path String containing a path to a directory, itself created with the \code{\link[alabaster.base]{saveObject}} method for \link[GenomicRanges]{GRangesList}s.
#' @param metadata Named list of metadata for this object, see \code{\link[alabaster.base]{readObjectFile}} for details.
#' @param ... Further arguments, to be passed to internal \code{\link[alabaster.base]{altReadObject}} calls.
#'
#' @return A \link[GenomicRanges]{GRangesList} object.
#'
#' @seealso
#' \code{"\link{saveObject,GRangesList-method}"}, to save an object to disk.
#'
#' @examples
#' gr <- GRanges(c("chrA", "chrB"), IRanges(c(1, 5), c(100, 200)))
#' seqlengths(gr) <- c(chrA=1000, chrB=2000)
#' grl <- split(gr, rep(1:3, length.out=length(gr)))
#'
#' tmp <- tempfile()
#' saveObject(grl, tmp)
#' readObject(tmp)
#' 
#' @export
#' @aliases loadGRangesList
readGRangesList <- function(path, metadata, ...) {
    .read_compressed_list(path, metadata, "genomic_ranges_list", ...)
}

##############################
######### OLD STUFF ##########
##############################

#' @export
loadGRangesList <- function(info, project) {
    .Deprecated(old = "loadGRangesList", new = "readGRangesList")
    concat.info <- acquireMetadata(project, info$genomic_ranges_list$concatenated$resource$path)
    concat <- .loadObject(concat.info, project=project)
    .load_compressed(concat, info, project)
}
