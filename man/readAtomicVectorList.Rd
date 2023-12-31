% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readAtomicVectorList.R
\name{readAtomicVectorList}
\alias{readAtomicVectorList}
\alias{loadAtomicVectorList}
\title{Load an atomic vector list}
\usage{
readAtomicVectorList(path, metadata, ...)
}
\arguments{
\item{path}{String containing a path to a directory, itself created with the \code{\link{saveObject}} method for \linkS4class{CompressedAtomicList}s.}

\item{metadata}{Named list of metadata for this object, see \code{\link{readObjectFile}} for details.}

\item{...}{Further arguments, to be passed to internal \code{\link{altReadObject}} calls.}
}
\value{
A CompressedAtomicList of the relevant type.
}
\description{
Load a list of atomic vectors as a \linkS4class{CompressedAtomicList} from its on-disk representation.
This is usually not directly called by users, but is instead called by dispatch in \code{\link{readObject}}.
}
\examples{
library(S4Vectors)
X <- splitAsList(LETTERS, sample(3, 26, replace=TRUE))

tmp <- tempfile()
saveObject(X, tmp)
readObject(tmp)

}
\seealso{
\code{"\link{saveObject,CompressedAtomicList-method}"}, to save an object to disk.
}
\author{
Aaron Lun
}
