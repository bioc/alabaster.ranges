\name{alabaster.ranges News}
\title{alabaster.ranges News}
\encoding{UTF-8}

\section{Version 1.14.0}{\itemize{
\item Officially deprecate \code{stageObject()} and \code{loadObject()} in favor of \code{saveObject()} and \code{readObject()}, respectively.
The former have been soft-deprecated and scrubbed from documentation for quite some time, so it is unlikely that anyone is using them.
}}

\section{Version 1.6.0}{\itemize{
\item Open HDF5 files in read-only mode to avoid permission-related problems for \code{readObject()}-dispatched functions.

\item Use \code{altSaveObject()} when saving child components inside \code{saveObject()} methods. 
}}
