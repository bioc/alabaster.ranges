#' Load a data frame list
#'
#' Load a list of data frames as a \linkS4class{CompressedSplitDataFrameList} from its on-disk representation.
#' This is usually not directly called by users, but is instead called by dispatch in \code{\link{readObject}}.
#'
#' @param path String containing a path to a directory, itself created with the \code{\link{saveObject}} method for \linkS4class{CompressedSplitDataFrameList} objects.
#' @param metadata Named list of metadata for this object, see \code{\link{readObjectFile}} for details.
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
#' readObject(tmp)
#'
#' @export
#' @aliases loadDataFrameList
readDataFrameList <- function(path, metadata, ...) {
    .read_compressed_list(path, metadata, "data_frame_list", ...)
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
