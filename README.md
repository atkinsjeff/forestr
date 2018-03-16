<!-- README.md is generated from README.Rmd. Please edit that file -->
forestr
-------

The forestr package calculates forest structure and canopy structure metrics from multiple data forms, including two-dimensional portable canopy LiDAR (PCL) raw data files and from certain processed three-dimensional terrestrial LiDAR scanner (TLS) data forms.

Installation
------------

To install version 1.0.1 which is on CRAN

``` r
install_packages("forestr")
```

To install the development version

``` r
devtools::install_github("atkinsjeff/forestr")
```

Usage
-----

To process raw PCL data, which comes in .csv form as two columns: column one is a string of numbers that represent return distance in meters, and column two is a string of integers that represent return intensity.

You can run an example data set that is included, a 40 m forest transect from Ordway-Swisher Biological Station in Hawthorn, FL.

The `process_pcl` function writes data to an output folder that is created in the working directory.

``` r

require(forestr)

process_pcl(osbs)
```

The output includes:

1.  an output.csv file that contains 24 canopy structral complexity (CSC) metrics including rumple, canopy rugosity, and max canopy height. These metrics are defined in [Hardiman et al. 2013](http://www.mdpi.com/1999-4907/4/3/537/htm) and in Atkins et al. (In review at Methods in Ecology and Evolution.

2.  an output\_hit\_matrix.csv file that is a file that contains the adjusted VAI by x and z position in the canopy.

3.  a summary\_matrix.csv file that gives the mean height, max heights, VAI and variance metrics by each columnar position, or x position along the transect.

Plotting
--------

The forestr package also produces hit grids--vegetation area index (VAI) for by 1 squared meter bins on the x and z axis through the canopy.

![](http://atkinsjeff.github.io/images/osbs.png)

Worked Example
--------------

Let's do a complete run using data from a red pine plantation in Michigan. These data are saved in the `data` folder as `red_pine.rda` in the master GitHub directory and can be accessed as `red_pine` and processed as such where we will look at all of the optional parameters:

``` r
red_pine <- "https://raw.githubusercontent.com/atkinsjeff/forestr/master/data-raw/red_pine_plain1.CSV"
forestr::process_pcl(red_pine, user_height = 1.05, marker.spacing = 10, max.vai = 8, pavd = TRUE, hist = TRUE )
#> how many in base df have NA
#> [1] 3446
#> Transect Length
#> [1] 30
#> Table of sky hits
#> 
#> FALSE  TRUE 
#> 14113  3446
#> RAW LiDAR metrics -- WARNING
#> Mean Return Height (m) of raw data
#> [1] 16.05084
#> Standard Deviation of raw  Canopy Height returns-- meanStd in old code
#> [1] 2.530479
#> Max Measured Canopy Height (m)
#> [1] 21.491
#> Scan Density
#> [1] 585.3
#> OPENNESS AND COVER METRICS
#> Sky Fraction (%)
#> [1] 19.62974
#> Cover Fraction (%)
#> [1] 80.37026
#> Rumple
#> [1] 1.8
#> Mean Gap Fraction ---as error check should be same as porosity
#> [1] 0.6848485
#> now we replace the 0's with 1's so when we take the ln they = 0
#> Clumping Index
#> [1] 0.5480317
#> Transect Length (m)
#> [1] 30
#> HEIGHT METRICS
#> Mean Leaf Height (H) - plot mean of column mean leaf height
#> [1] 16.47294
#> Height2 (H[2]) - standard deviation of column mean leaf height
#> [1] 1.658249
#> Mean Leaf Height variance (H[var]) - variance of column mean leaf height
#> [1] 2.749791
#> Root Mean Square Mean Leaf Height (H[rms]) - the root mean square or quadratic mean of column mean leaf height for the transect
#> [1] 16.55342
#> Max canopy height (m)
#> [1] 21.491
#> Mean Outer Canopy Height (m) or MOCH
#> [1] 18.96943
#> AREA AND DENSITY METRICS
#> Mean VAI - mean VAI for entire transect
#> [1] 4.166451
#> Mean Height of VAI[max] - modeEl
#> [1] 16.03333
#> Mode 2- The standard deviation of VAImax or MaxEl
#> [1] 2.722617
#> Maximum VAI for entire transect -- max el!
#> [1] 4.939786
#> Mean Peak VAI for entire transect
#> [1] 1.862675
#> CANOPY AND OPENNESS METRICS (cont.)
#> Deep Gaps
#> [1] 0
#> Deep Gap Fraction (0-1)
#> [1] 0
#> ARRANGEMENT METRICS
#> Canopy porosity
#> [1] 0.6848485
#> Square of leaf height variance (stdStd from old script)
#> [1] 19.35462
#> Mean Standard deviation of leaf heights -- meanStd
#> [1] 3.632249
#> Canopy Rugosity
#> [1] 2.482215
#> Surface Rugosity--TopRugosity
#> [1] 0.944426
#> Effective Number of Layers:
#> [1] 9.15834
#> [1] "red_pine_plain1_output"
#> [1] "./output/"
#> No. of NA values in hit matrix
#> [1] 452
```

Running the following command produces the following output to the console:

![](http://atkinsjeff.github.io/images/red_pine_hit_grid.png) ![](http://atkinsjeff.github.io/images/red_pine_pavd.png)
