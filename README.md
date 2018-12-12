
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wang13: Wang et al.’s (2013) Roughness Model

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The `wang13` R package implements Wang et al.’s (2013) roughness model.

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
roughness_wang(frequency_Hz = c(261.6, 523.3),
               level_dB = c(60, 60))
#> [1] 0.008891381
# Semitone
roughness_wang(frequency_Hz = c(261.6, 277.2),
               level_dB = c(60, 60))
#> [1] 0.2715607
```

## References

Wang, Y. S., Shen, G. Q., Guo, H., Tang, X. L., & Hamade, T. (2013).
Roughness modelling based on human auditory perception for sound quality
evaluation of vehicle interior noise. Journal of Sound and Vibration,
332(16), 3893–3904. <https://doi.org/10.1016/j.jsv.2013.02.030>
