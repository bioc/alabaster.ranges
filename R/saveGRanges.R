#' Save a GRanges object to disk
#'
#' Save a \linkS4class{GRanges} object to its on-disk representation .
#' 
#' @param x A \linkS4class{GRanges} object or one of its subclasses.
#' @inheritParams alabaster.base::saveObject
#'
#' @return 
#' \code{x} is saved to \code{path}, and \code{NULL} is invisibly returned.
#'
#' @seealso
#' \code{\link{readGRanges}}, to read a \linkS4class{GRanges} from disk.
#'
#' @examples
#' gr <- GRanges(c("chrA", "chrB"), IRanges(c(1, 5), c(100, 200)))
#' seqlengths(gr) <- c(chrA=1000, chrB=2000)
#'
#' tmp <- tempfile()
#' saveObject(gr, tmp)
#' list.files(tmp, recursive=TRUE)
#'
#' @author Aaron Lun
#' 
#' @export
#' @aliases stageObject,GRanges-method
#' @rdname saveGRanges
#' @import rhdf5 methods alabaster.base GenomicRanges
setMethod("saveObject", "GRanges", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)

    saveObject(seqinfo(x), file.path(path, "sequence_information"), ...)
    saveMetadata(
        x, 
        mcols.path=file.path(path, "range_annotations"),
        metadata.path=file.path(path, "other_annotations"), 
        ...
    )

    fpath <- file.path(path, "ranges.h5")
    h5createFile(fpath)
    name <- "genomic_ranges"
    h5createGroup(fpath, name)

    h5write(match(as.character(seqnames(x)), seqnames(seqinfo(x))) - 1L, fpath, paste0(name, "/sequence"))
    h5write(start(x), fpath, paste0(name, "/start"))
    h5write(width(x), fpath, paste0(name, "/width"))
    h5write(match(as.character(strand(x)), c("-", "*", "+")) - 1L, fpath, paste0(name, "/strand"))

    if (!is.null(names(x))) {
        h5write(names(x), fpath, paste0(name, "/name"))
    }

    write(file=file.path(path, "OBJECT"), name)
    invisible(NULL)
})

##############################
######### OLD STUFF ##########
##############################

#' @export
setMethod("stageObject", "GRanges", function(x, dir, path, child=FALSE, coord.name="ranges", seqinfo.name="seqinfo", mcols.name="mcols", meta.name="other") {
    dir.create(file.path(dir, path), showWarnings=FALSE)

    # Saving the sequence information.
    si.info <- tryCatch({
        meta <- .stageObject(seqinfo(x), dir, file.path(path, seqinfo.name), child=TRUE)
        .writeMetadata(dir=dir, meta)
    }, error=function(e) {
        stop("failed to stage 'seqinfo(<", class(x)[1], ">)'\n  - ", e$message)
    })

    # Saving other metadata. This automatically returns NULL if the names are NULL.
    range_data <- .processMcols(x, dir, path, mcols.name)
    other_data <- .processMetadata(x, dir, path, meta.name)

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
