#' Load a data frame list
#'
#' Load a list of data frames as a \linkS4class{CompressedSplitDataFrameList}, typically from files created by the corresponding \code{\link{stageObject}} method.
#'
#' @param path String containing a path to a directory, itself created with the \code{\link{saveObject}} method for \linkS4class{CompressedSplitDataFrameList} objects.
#' @param ... Further arguments, to be passed to internal \code{\link{altReadObject}} calls.
#'
#' @return A CompressedSplitDataFrameList.
#'
#' @seealso
#' \code{"\link{saveObject,CompressedSplitDataFrameList-method}"}, to save an object to disk.
#'
#' @author Aaron Lun
#'
#' @examples
#' library(S4Vectors)
#' Y <- splitAsList(DataFrame(Xxx=LETTERS, Yyy=1:26), sample(3, 26, replace=TRUE))
#'
#' tmp <- tempfile()
#' saveObject(Y, tmp)
#' readDataFrameList(tmp)
#'
#' @export
#' @aliases loadDataFrameList
readDataFrameList <- function(path, ...) {
    .read_compressed_list(path, "data_frame_list", ...)
}

##############################
######### OLD STUFF ##########
##############################

#' @export
loadDataFrameList <- function(info, project) {
    concat.info <- acquireMetadata(project, info$data_frame_list$concatenated$resource$path)
    concat <- .loadObject(concat.info, project=project)
    .load_compressed(concat, info, project=project)
}
