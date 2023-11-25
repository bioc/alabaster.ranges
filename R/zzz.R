.onLoad <- function(libname, pkgname) {
    registerReadObjectFunction("sequence_information", readSeqinfo)
    registerReadObjectFunction("genomic_ranges", readGRanges)
    registerReadObjectFunction("atomic_vector_list", readAtomicVectorList)
    registerReadObjectFunction("data_frame_list", readDataFrameList)
    registerReadObjectFunction("genomic_ranges_list", readGRangesList)
}

.onUnload <- function(libname, pkgname) {
    registerReadObjectFunction("sequence_information", NULL)
    registerReadObjectFunction("genomic_ranges", NULL)
    registerReadObjectFunction("atomic_vector_list", NULL)
    registerReadObjectFunction("data_frame_list", NULL)
    registerReadObjectFunction("genomic_ranges_list", NULL)
}
