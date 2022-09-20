# Save genomic ranges objects to file

The **alabaster.ranges** package implements methods for saving and loading `GRanges` and `GRangesList` objects under the **alabaster** framework.
It provides a language-agnostic method for serializing data in these gneomic coordinates as well as other compressed list.
To get started, install the package and its dependencies from GitHub:

```r
devtools::install_github("ArtifactDB/alabaster.schemas")
devtools::install_github("ArtifactDB/alabaster.base")
devtools::install_github("ArtifactDB/alabaster.ranges")
```

We can then save a `GRanges` to a file, preserving its `metadata` and `mcols`:

```r
library(GenomicRanges)
gr <- GRanges("chrA", IRanges(sample(100), width=sample(100)))
mcols(gr)$score <- runif(length(gr))
metadata(gr)$genome <- "Aaron"

library(alabaster.ranges)
tmp <- tempfile()
dir.create(tmp)
meta <- stageObject(gr, tmp, "gr")
meta[["$schema"]]
## [1] "genomic_ranges/v1.json"

roundtrip <- loadObject(meta, tmp)
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
dir.create(tmp)
meta <- stageObject(genes, tmp, "grl")
meta[["$schema"]]
## [1] "genomic_ranges_list/v1.json"

roundtrip <- loadObject(meta, tmp)
class(roundtrip)
## [1] "CompressedGRangesList"
## attr(,"package")
## [1] "GenomicRanges"
```
