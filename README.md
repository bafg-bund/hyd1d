
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hyd1d

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/hyd1d)](https://cran.r-project.org/package=hyd1d)
[![](https://cranlogs.r-pkg.org/badges/grand-total/hyd1d?color=green)](https://cran.r-project.org/package=hyd1d)
[![](https://cranlogs.r-pkg.org/badges/last-month/hyd1d?color=green)](https://cran.r-project.org/package=hyd1d)
[![](https://cranlogs.r-pkg.org/badges/last-week/hyd1d?color=green)](https://cran.r-project.org/package=hyd1d)
<!-- badges: end -->

The R package **hyd1d** is designed to compute 1-dimensional water level
information along the German Federal Waterways Elbe and Rhine.

## Installation

**hyd1d** is available from CRAN. To install it run:

``` r
install.packages("hyd1d")
```

To install the latest development version from Github run:

``` r
install.packages("devtools")
library(devtools)
devtools::install_github("bafg-bund/hyd1d")
```

## Usage

The package **hyd1d** is build around the S4-class
`WaterLevelDataFrame`. To compute and visualize 1-dimensional water
level information an object of class `WaterLevelDataFrame` has to be
initialized. Various functions included in **hyd1d** use these objects
and compute water levels stored in the column `w`.

``` r
# load the package
library(hyd1d)

# initialize a WaterLevelDataFrame
wldf <- WaterLevelDataFrame(river   = "Elbe",
                            time    = as.POSIXct("2016-12-21"),
                            station = seq(257, 262, 0.1))

# compute a water level
wldf <- waterLevel(wldf, TRUE)

# and plot it
plotShiny(wldf, TRUE, TRUE, TRUE, xlim = c(256.8, 262.2))
```

<img src="README_files/figure-gfm/usage-1.png" style="display: block; margin: auto;" />
