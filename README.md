# Save genomic ranges objects to file

The **alabaster.ranges** package implements methods for saving and loading `GRanges` and `GRangesList` objects under the **alabaster** framework.
It provides a language-agnostic method for serializing genomic coordinates in these objects, as well as data in related objects like compressed lists.
To get started, install the package and its dependencies from [Bioconductor](https://bioconductor.org/packages/alabaster.ranges):

```r
BiocManager::install("alabaster.ranges")
```

We can then save a `GRanges` to a file, preserving its `metadata` and `mcols`:

```r
library(GenomicRanges)
gr <- GRanges("chrA", IRanges(sample(100), width=sample(100)))
mcols(gr)$score <- runif(length(gr))
metadata(gr)$genome <- "Aaron"

library(alabaster.ranges)
tmp <- tempfile()
saveObject(gr, tmp)

roundtrip <- loadObject(tmp)
class(roundtrip)
## [1] "GRanges"
## attr(,"package")
## [1] "GenomicRanges"
```

Same for a `GRangesList` to a file:

```r
exons <- GRanges("chrA", IRanges(sample(100), width=sample(100)))
genes <- split(exons, sample(LETTERS, 100, replace=TRUE))
mcols(genes)$score <- runif(length(genes))
metadata(genes)$genome <- "Aaron"

library(alabaster.ranges)
tmp <- tempfile()
saveObject(genes, tmp)

roundtrip <- loadObject(tmp)
class(roundtrip)
## [1] "CompressedGRangesList"
## attr(,"package")
## [1] "GenomicRanges"
```
