
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EyeMetGenomics Package

<!-- badges: start -->
<!-- badges: end -->
<p align="justify">

EyeMetGenomics is a R package that performs an ….<br>

</p>

## Installation

Once R (version \> “4.0”) has been started, you can install the released
version of EyeMetGenomics Package from GitHub with

``` r
devtools::install_github("marziasettino/EyeMetGenomics", build_vignettes = TRUE)
library(EyeMetGenomics)
```

All the plots produced by the EyeMetGenomics functions will be saved by
default in the directory **“ResultsPlot”** that therefore needs to be
created.

## Required libraries

``` r
library(dplyr)
library(DT)
library(ggplot2)
library(stringr)
library(ggpubr)
library(caret)
```

### Use-case diagram of EyeMetGenomics

<div class="panel panel-info">

<div class="panel-heading">

The use-case diagram represents the high-level functionalities of
EyeMetGenomics <br>

</div>

<div class="panel-body">

</div>

</div>

## Vignettes

A list of all currently integrated vignettes can be obtained through:

``` r
vignette(package="EyeMetGenomics")
```

The best way to view vignettes is in your web browser:

``` r
devtools::load_all(".")
browseVignettes("EyeMetGenomics")
```

Get the list of the example data sets

``` r
data(package = "EyeMetGenomics")
```
