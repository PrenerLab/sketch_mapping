Analyze Clusters
================
Christopher Prener, Ph.D.
(February 27, 2020)

## Introduction

This notebook calculates Moran’s \(I\) statistics for both the frequent
and regular data.

## Dependencies

This notebook requires the following packages:

``` r
# tidyverse packages
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# spatial packages
library(spdep)
```

    ## Loading required package: sp

    ## Loading required package: spData

    ## To access larger datasets in this package, install the spDataLarge
    ## package with: `install.packages('spDataLarge',
    ## repos='https://nowosad.github.io/drat/', type='source')`

    ## Loading required package: sf

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

``` r
library(sf)

# other packages
library(here)
```

    ## here() starts at /Users/prenercg/GitHub/PrenerLab/sketch_mapping

## Load and Prepare Data

We’ll load our data and papre it by converting it to an `sp` object.
First, the frequent
data:

``` r
freq_respondents <- st_read(here("data", "summaries", "freq_respondents"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `freq_respondents' from data source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/freq_respondents' using driver `ESRI Shapefile'
    ## Simple feature collection with 49 features and 5 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 885839.8 ymin: 1011334 xmax: 900154.4 ymax: 1025649
    ## epsg (SRID):    NA
    ## proj4string:    +proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs

``` r
freq_respondents <- mutate(freq_respondents, frequent_pct = frequent/53*100)

freq_respondents <- as(freq_respondents, Class = "Spatial")
```

Then the regular
data:

``` r
reg_respondents <- st_read(here("data", "summaries", "reg_respondents"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `reg_respondents' from data source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/reg_respondents' using driver `ESRI Shapefile'
    ## Simple feature collection with 49 features and 5 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 885839.8 ymin: 1011334 xmax: 900154.4 ymax: 1025649
    ## epsg (SRID):    NA
    ## proj4string:    +proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs

``` r
reg_respondents <- mutate(reg_respondents, regular_pct = regular/53*100)

reg_respondents <- as(reg_respondents, Class = "Spatial")
```

## Analyze Frequent Data

We need to create a spatial weights object (using rook) for our data:

``` r
nb <- poly2nb(freq_respondents, queen=FALSE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
```

With a weights object created, we can run a Moran’s I test:

``` r
moran.test(freq_respondents$frequent_pct,lw)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  freq_respondents$frequent_pct  
    ## weights: lw    
    ## 
    ## Moran I statistic standard deviate = 5.6002, p-value = 1.071e-08
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.515443274      -0.020833333       0.009170103

And then finally use a monte carlo simulation to estimate the \(p\)
value for our test:

``` r
moran.mc(freq_respondents$frequent_pct, lw, nsim=999)
```

    ## 
    ##  Monte-Carlo simulation of Moran I
    ## 
    ## data:  freq_respondents$frequent_pct 
    ## weights: lw  
    ## number of simulations + 1: 1000 
    ## 
    ## statistic = 0.51544, observed rank = 1000, p-value = 0.001
    ## alternative hypothesis: greater

## Analyze Regular Data

We need to create a spatial weights object (using rook) for our data:

``` r
nb <- poly2nb(reg_respondents, queen=FALSE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
```

With a weights object created, we can run a Moran’s I test:

``` r
moran.test(reg_respondents$regular_pct,lw)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  reg_respondents$regular_pct  
    ## weights: lw    
    ## 
    ## Moran I statistic standard deviate = 5.318, p-value = 5.246e-08
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##        0.53644679       -0.02083333        0.01098127

And then finally use a monte carlo simulation to estimate the \(p\)
value for our test:

``` r
moran.mc(reg_respondents$regular_pct, lw, nsim=999)
```

    ## 
    ##  Monte-Carlo simulation of Moran I
    ## 
    ## data:  reg_respondents$regular_pct 
    ## weights: lw  
    ## number of simulations + 1: 1000 
    ## 
    ## statistic = 0.53645, observed rank = 1000, p-value = 0.001
    ## alternative hypothesis: greater

## Clean-up Enviornment

Finally, we’ll clean-up our enviornment:

``` r
rm(freq_respondents, reg_respondents, lw, nb)
```
