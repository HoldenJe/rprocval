
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rprocval

<!-- badges: start -->

![R
badge](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Github
badge](https://img.shields.io/badge/GitHub-100000?style=for-the-badge&logo=github&logoColor=white)
<br> ![develoment
badge](https://img.shields.io/badge/Dev-InProgress-%3CCOLOR%3E.svg)
<!-- badges: end -->

# Introduction

**Package currently in development**

The goal of `rprocval` is to provide a series of queries to help
identify potential errors in fisheries data. Most functions are designed
to operate on relational databases with a structure consistent with the
standard FishNet2 data model.

## Installation

You can install `rprocval` like so:

``` r
devtools::install_github("HoldenJe/rprocval")
```

## Bugs and Feature Requests

Please file a github issue if you encounter a bug or have an idea for a
feature request. This approach provides a notification to the code
maintainers and allows tracking of commits related to specific tasks.

## How to Use

There are two types of function currently utilized within the package:  
1. Data structure queries (e.g. `fn121_column_check`,
`fn123_error_queries`)  
2. Data quality queries (e.g. `fn125_fl_gt_tl`, `fn125_vonb_check`)

Data structure queries provide checks for orphaned records or
inconsistencies between tables or conventions such as failing to convert
a total catch weight (CATWT) of a net to kg. The data quality queries
focus on individual fish attributes and generally use fitted models to
identify outliers using a log ratio threshold between the observed and
predicted values. The general naming convention of all functions is to
start the function name with the primary table the function will check.
Note however that the function may have to access data from another
table to perform the tests.

The functions are designed to be run interactively (as opposed to a
batch process). Tests are generally designed to be species specific and
thus it is either necessary to run the tests on a subset of species
specific data or preferably, split the data by species in to a list and
use `purrr::map` to apply the function across all species.

``` r
myoutput <- FN125 %>%
  split(.$SPC) %>%
  map(fn125_tl_rwt_lm, makeplot = T) %>%
  map(fn125_fl_rwt_lm, makeplot = T)
```

This interactive approach allows users to correct errors in the data and
then easily re-run the functions. For instance, an outlier may prevent
one of the `nls` models from converging and thus identifying any
outliers. Such outliers should be immediately apparent when the
associated plot is viewed. This error should be corrected or removed and
then it is likely that successive tests will provide meaningful results.
Similarly, this approach allows users to interactively test methods of
correcting the data by applying the correction within the R environment
prior to making a change to the data record and then testing whether the
applied changes result in records passing the necessary checks. This
approach works well for errors in units (e.g. g vs kg as an expected
unit) or a transcription error that resulted in FLEN values in the TLEN
column (and thus FLEN \> TLEN).

Most of the `fn125_*` functions have 2 optional arguments: `makeplot`
and `fail_criteria`. The `makeplot` argument determines whether plots
should be made as an output of the running the function. It is useful to
view these plots but is not always necessary and can often slow the
processing time down considerably. The `fail_criteria` has default
values set for each specific test. If records that are suspected errors
are not being flagged it is possible to provide a more restrictive
(smaller value) criteria through the use of this argument. Details of
the `fail_criteria` can be found on the help pages for each function.
