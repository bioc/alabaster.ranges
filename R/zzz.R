.onLoad <- function(libname, pkgname) {
    registerValidateObjectFunction("sequence_information", function(path) {})
    registerValidateObjectFunction("genomic_ranges", function(path) {})
    registerValidateObjectFunction("atomic_vector_list", function(path) {})
    registerValidateObjectFunction("data_frame_list", function(path) {})
    registerValidateObjectFunction("genomic_ranges_list", function(path) {})

    registerReadObjectFunction("sequence_information", readSeqinfo)
    registerReadObjectFunction("genomic_ranges", readGRanges)
    registerReadObjectFunction("atomic_vector_list", readAtomicVectorList)
    registerReadObjectFunction("data_frame_list", readDataFrameList)
    registerReadObjectFunction("genomic_ranges_list", readGRangesList)
}

.onUnload <- function(libname, pkgname) {
    registerValidateObjectFunction("sequence_information", NULL)
    registerValidateObjectFunction("genomic_ranges", NULL)
    registerValidateObjectFunction("atomic_vector_list", NULL)
    registerValidateObjectFunction("data_frame_list", NULL)
    registerValidateObjectFunction("genomic_ranges_list", NULL)

    registerReadObjectFunction("sequence_information", NULL)
    registerReadObjectFunction("genomic_ranges", NULL)
    registerReadObjectFunction("atomic_vector_list", NULL)
    registerReadObjectFunction("data_frame_list", NULL)
    registerReadObjectFunction("genomic_ranges_list", NULL)
}
