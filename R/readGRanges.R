#' Read a GRanges from disk
#'
#' Read a \linkS4class{GRanges} object from its on-disk representation.
#' 
#' @param path String containing a path to a directory, itself created with the \code{\link{stageObject}} method for \linkS4class{GRanges}.
#' @param ... Further arguments to pass to internal \code{\link{altReadObject}} calls.
#'
#' @return A \linkS4class{GRanges} object.
#'
#' @seealso
#' \code{"\link{saveObject,GRanges-method}"}, to save a \linkS4class{GRanges} to disk.
#'
#' @examples
#' gr <- GRanges(c("chrA", "chrB"), IRanges(c(1, 5), c(100, 200)))
#' seqlengths(gr) <- c(chrA=1000, chrB=2000)
#'
#' tmp <- tempfile()
#' saveObject(gr, tmp)
#' readGRanges(tmp)
#' 
#' @export
#' @aliases loadGRanges
#' @import rhdf5 alabaster.base IRanges GenomicRanges
readGRanges <- function(path, ...) {
    si <- altReadObject(file.path(path, "sequence_information"), ...)

    fpath <- file.path(path, "ranges.h5")
    fhandle <- H5Fopen(fpath)
    on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)
    ghandle <- H5Gopen(fhandle, "genomic_ranges")
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    x <- GRanges(
        seqnames(si)[h5_read_vector(ghandle, "sequence") + 1L],
        IRanges(
            h5_read_vector(ghandle, "start"),
            width=h5_read_vector(ghandle, "width")
        ),
        c("-", "*", "+")[h5_read_vector(ghandle, "strand") + 2L],
        seqinfo=si
    )

    if (h5_object_exists(ghandle, "name")) {
        names(x) <- h5_read_vector(ghandle, "name")
    }

    readMetadata(x, 
        mcols.path=file.path(path, "range_annotations"),
        metadata.path=file.path(path, "other_annotations"),
        ...
    )
}

##############################
######### OLD STUFF ##########
##############################

#' @export
loadGRanges <- function(info, project) {
    # First, pulling out the seqinfo.
    si.info <- acquireMetadata(project, info$genomic_ranges$sequence_information$resource$path)
    SI <- .loadObject(si.info, project)

    # Convert the CSV into a GRanges.
    path <- acquireFile(project, info$path)
    gr.has.names <- isTRUE(info$genomic_ranges$names)

    rr.df <- .quickReadCsv(path, 
        expected.columns = c(seqnames="character", start="integer", end="integer", strand="character"),
        row.names = gr.has.names,
        compression = info$genomic_ranges$compression, 
        expected.nrows = info$genomic_ranges$length
    )

    rr <- GRanges(rr.df$seqnames, IRanges(rr.df$start, rr.df$end), strand=rr.df$strand, seqinfo=SI)
    if (gr.has.names) {
        names(rr) <- rownames(rr.df)
    } 

    .restoreMetadata(rr, mcol.data=info$genomic_ranges$range_data, meta.data=info$genomic_ranges$other_data, project=project)
}
