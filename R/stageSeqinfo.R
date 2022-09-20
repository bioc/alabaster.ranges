#' Stage a Seqinfo object
#'
#' Stage a \linkS4class{Seqinfo} object containing genomic information. 
#'
#' @param x A \linkS4class{Seqinfo} object. 
#' @inheritParams alabaster.base::stageObject
#'
#' @return 
#' A named list containing the metadata for \code{x}.
#' The contents of \code{x} are saved into various files inside \code{file.path(dir, path)}.
#'
#' @examples
#' si <- Seqinfo(c("chrA", "chrB"), c(1000, 2000))
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' stageObject(si, tmp, path="seqinfo")
#' list.files(tmp, recursive=TRUE)
#'
#' @export
#' @rdname stageSeqinfo
setMethod("stageObject", "Seqinfo", function(x, dir, path, child=FALSE) {
    dir.create(file.path(dir, path), showWarnings=FALSE)

    si.path <- file.path(path, "simple.csv.gz")
    x <- as.data.frame(x)
    si <- cbind(seqnames=rownames(x), x)
    .quickWriteCsv(si, path=file.path(dir, si.path))

    list(
        `$schema`="sequence_information/v1.json",
        path=si.path,
        is_child=child,
        sequence_information=list(
            dimensions=dim(x),
            compression="gzip"
        )
    )
})
