#' Load a GRanges
#'
#' Load a \linkS4class{GRanges} object based on the metadata saved by the corresponding \code{\link{stageObject}} method.
#' 
#' @inheritParams alabaster.base::loadDataFrame
#'
#' @return A \linkS4class{GRanges} object.
#'
#' @examples
#' gr <- GRanges(c("chrA", "chrB"), IRanges(c(1, 5), c(100, 200)))
#' seqlengths(gr) <- c(chrA=1000, chrB=2000)
#'
#' # Staging it:
#' tmp <- tempfile()
#' dir.create(tmp)
#' info <- stageObject(gr, tmp, path="ranges")
#'
#' # Now loading it back in:
#' loadGRanges(info, tmp)
#' 
#' @export
#' @rdname loadGRanges
#' @importFrom IRanges IRanges
#' @importFrom GenomicRanges GRanges
#' @importFrom GenomeInfoDb Seqinfo
#' @importFrom utils read.csv head
loadGRanges <- function(info, project) {
    # First, pulling out the seqinfo.
    si.info <- acquireMetadata(project, info$genomic_ranges$sequence_information$resource$path)
    SI <- .loadObject(si.info, project)

    # Convert the CSV into a GRanges.
    path <- acquireFile(project, info$path)
    gr.has.names <- !is.null(info$genomic_ranges$names)

    rr.df <- .quickReadCsv(path, 
        expected.columns = c(seqnames="character", start="integer", end="integer", strand="character"),
        row.names = gr.has.names,
        compression = info$genomic_ranges$compression, 
        expected.nrows = info$genomic_ranges$length
    )

    rr <- GRanges(rr.df$seqnames, IRanges(rr.df$start, rr.df$end), strand=rr.df$strand, seqinfo=SI)
    if (gr.has.names) {
        names(rr) <- rownames(rr.df)
    } 

    .restoreMetadata(rr, mcol.data=info$genomic_ranges$range_data, meta.data=info$genomic_ranges$other_data, project=project)
}
