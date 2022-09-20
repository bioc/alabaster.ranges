#' Load a GRanges
#'
#' Load a \linkS4class{GRangesList} object based on the metadata saved by the corresponding \code{\link{stageObject}} method.
#' 
#' @inheritParams alabaster.base::loadDataFrame
#'
#' @return A \linkS4class{GRangesList} object.
#'
#' @examples
#' gr <- GRanges(c("chrA", "chrB"), IRanges(c(1, 5), c(100, 200)))
#' seqlengths(gr) <- c(chrA=1000, chrB=2000)
#' grl <- split(gr, rep(1:3, length.out=length(gr)))
#'
#' # Staging it:
#' tmp <- tempfile()
#' dir.create(tmp)
#' info <- stageObject(grl, tmp, path="ranges")
#'
#' # Now loading it back in:
#' loadGRangesList(info, tmp)
#' 
#' @export
#' @import alabaster.base
loadGRangesList <- function(info, project) {
    concat.info <- acquireMetadata(project, info$genomic_ranges_list$concatenated$resource$path)
    concat <- .loadObject(concat.info, project=project)
    .load_compressed(concat, info, project)
}
