.validatedStage <- function(x, dir, ...) {
    info <- stageObject(x, dir, ...)
    .writeMetadata(info, dir)
    info
}
