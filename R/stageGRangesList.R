#' Stage a GRangesList object
#'
#' Stage a \linkS4class{GRangesList} object containing groups of genomic intervals.
#' 
#' @param x A \linkS4class{GRangesList} object or one of its subclasses.
#' @inheritParams alabaster.base::stageObject
#' @param group.name String containing the name of the file inside \code{path} to save the GRangesList groupings.
#' @param ranges.name String containing the name of the directory inside \code{path} to save the underlying GRanges (i.e., \code{\link{unlist}(x)}).
#' @param ranges.args Further arguments to pass to the \code{\link{stageObject}} method when saving the underlying GRanges.
#' @param mcols.name String specifying the name of the directory inside \code{path} to save \code{\link{mcols}(x)}.
#' If \code{NULL}, per-element metadata is not saved.
#' @param meta.name String specifying the name of the directory inside \code{path} to save \code{\link{metadata}(x)}.
#' If \code{NULL}, object metadata is not saved.
#'
#' @return 
#' A named list containing the metadata for \code{x}.
#' The contents of \code{x} are saved into various files inside \code{file.path(dir, path)}.
#'
#' @details
#' Setting \code{mcols.name=NULL} and \code{meta.name=NULL} will skip the staging of the \code{\link{mcols}} and \code{\link{metadata}}.
#' This is primarily useful for use in staging RangedSummarizedExperiments where the \code{\link{mcols}} have already been saved as part of the \code{rowData}.
#'
#' @examples
#' gr <- GRanges("chrA", IRanges(1:100, width=1))
#' grl <- split(gr, rep(1:3, length.out=length(gr)))
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' stageObject(grl, tmp, path="GRL")
#' list.files(tmp, recursive=TRUE)
#'
#' @author Aaron Lun
#' 
#' @export
#' @importFrom GenomeInfoDb seqinfo
#' @rdname stageGRangesList
setMethod("stageObject", "GRangesList", function(x, dir, path, child=FALSE, group.name="grouping", mcols.name="mcols", ranges.name="ranges", meta.name="other", ranges.args=list()) {
    .compressed_stager(x, 
        concatenated=unlist(x, use.names=FALSE),
        schema="genomic_ranges_list/v1.json",
        dir=dir, 
        path=path, 
        child=child,
        group.name=group.name, 
        concat.name=ranges.name, 
        mcols.name=mcols.name,
        meta.name=meta.name,
        extra.args=ranges.args)
})
