ShinyDSP is a Shiny app that lets you interactively visualize Nanostring GeoMx
data. Its outputs are highly customizable. It performs differential gene
expression analysis and summarizes the results in tables and plots. 
Please refer to the vignette for details. 

## Installation

ShinyDSP is available on Bioconductor and can be installed with the following:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("ShinyDSP")

# To install the development version from Github:
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")

devtools::install_github("kimsjune/ShinyDSP")
```

## Usage

```r
library(ShinyDSP)
app <- ShinyDSP()
if (interactive()) shiny::runApp(app)
```
