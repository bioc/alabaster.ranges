#' Save a GRangesList object to disk
#'
#' Save a \linkS4class{GRangesList} object to its on-disk representation.
#' 
#' @param x A \linkS4class{GRangesList} object. 
#' @inheritParams alabaster.base::saveObject
#'
#' @return 
#' \code{x} is saved to \code{path}, and \code{NULL} is invisibly returned.
#'
#' @seealso
#' \code{\link{readGRangesList}}, to read a \linkS4class{GRangesList} from disk.
#'
#' @examples
#' gr <- GRanges("chrA", IRanges(1:100, width=1))
#' grl <- split(gr, rep(1:3, length.out=length(gr)))
#'
#' tmp <- tempfile()
#' saveObject(grl, tmp)
#' list.files(tmp, recursive=TRUE)
#'
#' @author Aaron Lun
#' 
#' @export
#' @aliases stageObject,GRangesList-method
#' @rdname saveGRangesList
setMethod("saveObject", "GRangesList", function(x, path, ...) .save_compressed_list(x, path=path, name="genomic_ranges_list", ...))

##############################
######### OLD STUFF ##########
##############################

#' @export
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
