
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wang13: Wang et al.’s (2013) Roughness Model

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/pmcharrison/wang13.svg?branch=master)](https://travis-ci.org/pmcharrison/wang13)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/wang13?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/wang13)
[![Coverage
status](https://coveralls.io/repos/github/pmcharrison/wang13/badge.svg)](https://coveralls.io/r/pmcharrison/wang13?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2545764.svg)](https://doi.org/10.5281/zenodo.2545764)

The `wang13` R package implements Wang et al.’s (2013) roughness model.

Try an interactive demo [here](http://shiny.pmcharrison.com/wang13/)\!

## Installation

Within R, you can install the current version of `wang13` from Github as
follows:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("wang13")
```

## Usage

Higher scores correspond to greater roughness.

``` r
library(wang13)

# Octave
roughness_wang(c(60, 72))
#> [1] 0.2005657

# Semitone
roughness_wang(c(60, 61))
#> [1] 1.124933
```

## References

Wang, Y. S., Shen, G. Q., Guo, H., Tang, X. L., & Hamade, T. (2013).
Roughness modelling based on human auditory perception for sound quality
evaluation of vehicle interior noise. Journal of Sound and Vibration,
332(16), 3893–3904. <https://doi.org/10.1016/j.jsv.2013.02.030>
