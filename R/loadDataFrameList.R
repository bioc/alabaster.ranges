#' Load a data frame list
#'
#' Load a list of data frames as a \linkS4class{CompressedSplitDataFrameList}, typically from files created by the corresponding \code{\link{stageObject}} method.
#'
#' @inheritParams alabaster.base::loadDataFrame
#'
#' @return A CompressedSplitDataFrameList.
#'
#' @author Aaron Lun
#'
#' @examples
#' Y <- splitAsList(DataFrame(Xxx=LETTERS, Yyy=1:26), sample(3, 26, replace=TRUE))
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' info <- stageObject(Y, tmp, path="test2")
#'
#' loadDataFrameList(info, tmp)
#'
#' @export
loadDataFrameList <- function(info, project) {
    concat.info <- acquireMetadata(project, info$data_frame_list$concatenated$resource$path)
    concat <- .loadObject(concat.info, project=project)
    .load_compressed(concat, info, project=project)
}
