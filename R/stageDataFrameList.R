#' Stage compressed lists of DataFrames
#'
#' Stage \linkS4class{CompressedSplitDataFrameList} objects.
#'
#' @param x A CompressedList containing DataFrames with the same columns.
#' @inheritParams alabaster.base::stageObject
#' @param group.name String containing the name of the file inside \code{path} to save the list groupings.
#' @param concat.name String containing the extension-less name of the file inside \code{path} to save the concatenated elements.
#' @param mcols.name String specifying the name of the directory inside \code{path} to save the \code{\link{mcols}}.
#' If \code{NULL}, the metadata columns are not saved.
#' @param meta.name String specifying the name of the directory inside \code{path} to save \code{\link{metadata}(x)}.
#' If \code{NULL}, object metadata is not saved.
#'
#' @return 
#' A named list containing the metadata for \code{x}.
#' The contents of \code{x} are saved inside \code{path} and referenced from the metadata. 
#'
#' @details
#' The staging process will save both the interval groupings and the concatenated DataFrame into separate files.
#' The CompressedList may also be decorated with metadata for each list element in its \code{\link{mcols}}, which is saved to \code{mcols.name}.
#' No file is created at \code{mcols.name} if \code{\link{mcols}(x)} is \code{NULL} or has no columns.
#'
#' @author Aaron Lun
#' @examples
#' tmp <- tempfile()
#' dir.create(tmp)
#' 
#' library(S4Vectors)
#' Y <- splitAsList(DataFrame(Xxx=LETTERS, Yyy=1:26), sample(3, 26, replace=TRUE))
#' stageObject(Y, tmp, path="test2")
#' list.files(file.path(tmp, "test2"))
#'
#' @export
#' @importFrom BiocGenerics unlist
#' @rdname stageDataFrameList
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
