
<!-- README.md is generated from README.Rmd. Please edit that file -->
hyd1d
=====

The R package **hyd1d** is designed to compute 1-dimensional water level information along German federal waterways Elbe and Rhine.

Installation
------------

**hyd1d** is not currently available from CRAN, but you can install the development version from BfG's gitbucket server with:

``` r
# install.packages("devtools")
devtools::install_git("git://apps.bafg.de/gitbucket/webera/hyd1d.git")
```

Usage
-----

The package **hyd1d** is build around the S4-class `WaterLevelDataFrame`. To compute and visualize 1-dimensional water level information an object of class `WaterLevelDataFrame` has to be initialized. Various functions included in **hyd1d** use these objects and compute water levels stored in the column `w`.

``` r
# load the package
require(hyd1d)

# initialize a WaterLevelDataFrame
wldf <- WaterLevelDataFrame(river   = "Elbe",
                            time    = as.POSIXct("2016-12-21"),
                            station = seq(257, 262, 0.1))

# compute a water level
wldf <- waterLevel(wldf, TRUE)

# and plot it
plotShiny(wldf, TRUE, TRUE, TRUE)
```

<img src="README_files/figure-markdown_github/usage-1.png" style="display: block; margin: auto;" />
