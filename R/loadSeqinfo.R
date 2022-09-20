#' Load a Seqinfo
#'
#' Load a \linkS4class{Seqinfo} object based on the metadata saved by the corresponding \code{\link{stageObject}} method.
#' 
#' @inheritParams alabaster.base::loadDataFrame
#'
#' @return A \linkS4class{Seqinfo} object.
#'
#' @examples
#' si <- Seqinfo(c("chrA", "chrB"), c(1000, 2000))
#'
#' # Staging it:
#' tmp <- tempfile()
#' dir.create(tmp)
#' info <- stageObject(si, tmp, path="seqinfo")
#'
#' # Now loading it back in:
#' loadSeqinfo(info, tmp)
#' 
#' @export
#' @importFrom alabaster.base acquireFile
#' @importFrom GenomeInfoDb Seqinfo
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
