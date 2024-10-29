# shinyDSP

<!-- badges: start -->

[![check-bioc](https://github.com/kimsjune/shinyDSP/actions/workflows/check-bioc.yml/badge.svg)](https://github.com/kimsjune/shinyDSP/actions/workflows/check-bioc.yml)

<!-- badges: end -->


shinyDSP is a Shiny app that lets you interactively visualize Nanostring GeoMx
data. Its outputs are highly customizable. It performs differential gene
expression analysis and summarizes the results in tables and plots. 
Please refer to the vignette for details. 

## Installation

shinyDSP is available on Bioconductor and can be installed with the following:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("shinyDSP")

# To install the development version from Github:
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")

devtools::install_github("kimsjune/shinyDSP")
```

## Usage

```r
library(shinyDSP)
app <- shinyDSP()
if (interactive()) shiny::runApp(app)
```
