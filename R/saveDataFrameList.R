#' Save compressed lists of data frames to disk
#'
#' Save a \linkS4class{CompressedSplitDataFrameList} object to its on-disk representation.
#'
#' @param x A \linkS4class{CompressedSplitDataFrameList} object.
#' @inheritParams alabaster.base::saveObject
#'
#' @return 
#' \code{x} is saved to \code{path}, and \code{NULL} is invisibly returned.
#'
#' @seealso
#' \code{\link{readDataFrameList}}, to read a \linkS4class{CompressedSplitDataFrameList} from disk.
#'
#' @author Aaron Lun
#' @examples
#' library(S4Vectors)
#' Y <- splitAsList(DataFrame(Xxx=LETTERS, Yyy=1:26), sample(3, 26, replace=TRUE))
#'
#' tmp <- tempfile()
#' saveObject(Y, tmp)
#' list.files(tmp, recursive=TRUE)
#'
#' @export
#' @aliases stageObject,CompressedSplitDataFrameList-method
#' @rdname saveCompressedSplitDataFrameList
setMethod("saveObject", "CompressedSplitDataFrameList", function(x, path, ...) .save_compressed_list(x, path=path, name="data_frame_list", ...))

##############################
######### OLD STUFF ##########
##############################

#' @export
setMethod("stageObject", "CompressedSplitDataFrameList", function(x, dir, path, child=FALSE, group.name="grouping.csv", concat.name="concatenated", mcols.name="mcols", meta.name="other") {
    rd <- unlist(x, use.names=FALSE)
    .compressed_stager(x, 
        concatenated=rd, 
        schema="data_frame_list/v1.json",
        dir=dir, 
        path=path, 
        child=child,
        group.name=group.name, 
        concat.name=concat.name, 
        mcols.name=mcols.name,
        meta.name=meta.name
    )
})
