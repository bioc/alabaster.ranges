#' Stage a GRanges object
#'
#' Stage a \linkS4class{GRanges} object containing genomic intervals.
#' 
#' @param x A \linkS4class{GRanges} object or one of its subclasses.
#' @inheritParams alabaster.base::stageObject
#' @param coord.name String containing the name of the file inside \code{path} to save the genomic coordinates.
#' @param seqinfo.name String containing the name of the file inside \code{path} to save the sequence information.
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
#' gr <- GRanges(c("chrA", "chrB"), IRanges(c(1, 5), c(100, 200)))
#' seqlengths(gr) <- c(chrA=1000, chrB=2000)
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' stageObject(gr, tmp, path="ranges")
#' list.files(tmp, recursive=TRUE)
#'
#' @author Aaron Lun
#' 
#' @export
#' @import alabaster.base
#' @import methods
#' @importFrom GenomeInfoDb seqinfo
#' @importFrom S4Vectors mcols<-
#' @rdname stageGRanges
setMethod("stageObject", "GRanges", function(x, dir, path, child=FALSE, coord.name="ranges", seqinfo.name="seqinfo", mcols.name="mcols", meta.name="other") {
    dir.create(file.path(dir, path), showWarnings=FALSE)

    # Saving the sequence information.
    si.info <- tryCatch({
        meta <- .stageObject(seqinfo(x), dir, file.path(path, seqinfo.name), child=TRUE)
        .writeMetadata(dir=dir, meta)
    }, error=function(e) {
        stop("failed to stage 'seqinfo(<", class(x)[1], ">)'\n  - ", e$message)
    })

    # Saving other metadata.
    range_data <- NULL
    if (!is.null(mcols.name)) {
        range_data <- .processMcols(x, dir, path, mcols.name)
    }

    other_data <- NULL
    if (!is.null(meta.name)) {
        other_data <- .processMetadata(x, dir, path, meta.name)
    }

    # Saving the GRanges (removing the metadata first, otherwise we'd get extra columns).
    mcols(x) <- NULL

    df <- as.data.frame(x)
    if (!is.character(df$seqnames) && !is.factor(df$seqnames)) {
        stop("'seqnames' from 'as.data.frame(<GRanges>)' should be a character vector or factor")
    }
    if (!is.integer(df$start)) {
        stop("'start' from 'as.data.frame(<GRanges>)' should be an integer vector")
    }
    if (!is.integer(df$end)) {
        stop("'end' from 'as.data.frame(<GRanges>)' should be an integer vector")
    }
    if (!is.character(df$strand) && !is.factor(df$strand)) {
        stop("'strand' from 'as.data.frame(<GRanges>)' should be a character vector")
    }
    df <- df[,c("seqnames", "start", "end", "strand"),drop=FALSE] # protect against subclasses that add extra stuff.

    if (!is.null(names(x))) {
        df <- cbind(row_names=names(x), df)
    }
    gr.path <- file.path(path, paste0(coord.name, ".csv.gz"))
    gr.file <- file.path(dir, gr.path)
    .quickWriteCsv(df, path=gr.file)

    list(
        `$schema`="genomic_ranges/v1.json",
        path=gr.path,
        is_child=child,
        genomic_ranges=list(
            length=NROW(x),
            names=!is.null(names(x)),
            sequence_information=list(resource=si.info),
            range_data=range_data,
            other_data=other_data,
            compression="gzip"
        )
    )
})
