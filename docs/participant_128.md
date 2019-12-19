Participant 128
================
Christopher Prener, Ph.D.
(December 19, 2019)

## Introduction

This notebook is used for digitizing participant data for the `qualmap`
pilot project.

## Dependencies

This notebook requires the following packages:

``` r
# spatial packages
library(qualmap)
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

``` r
# tidyverse packages
library(readr)

# other packages
library(here)
```

    ## here() starts at /Users/prenercg/GitHub/PrenerLab/sketch_mapping

``` r
library(testthat)
```

## Load Reference Data

First, we’ll load the reference data from
    `data/grids/`:

``` r
grids <- st_read(here("data", "grid"), stringsAsFactors = FALSE)
```

    ## Reading layer `grid' from data source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/grid' using driver `ESRI Shapefile'
    ## Simple feature collection with 63 features and 2 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 883794.9 ymin: 1011334 xmax: 902199.3 ymax: 1025649
    ## epsg (SRID):    NA
    ## proj4string:    +proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs

## Define and Validate Participant Clusters

First, we’ll define the clusters associated with the “frequent” prompt:

``` r
cluster1 <- qm_define("F-3")
cluster2 <- qm_define("F-3")
cluster3 <- qm_define("F-3", "G-3")
cluster4 <- qm_define("G-3")
cluster5 <- qm_define("F-5")
```

Next, we’ll validate these data. The validation is performed in the
context of a unit test to ensure that each object is
valid:

``` r
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster1), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster2), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster3), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster4), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster5), TRUE)
```

No output on these unit tests means that each cluster is indeed valid.

Next, we’ll repeat the process with the “regular” prompt by first
entering the cluster data:

``` r
cluster6 <- qm_define("F-3")
cluster7 <- qm_define("F-3", "G-3")
cluster8 <- qm_define("F-3", "G-3")
cluster9 <- qm_define("G-3")
cluster10 <- qm_define("F-5")
```

As before, we’ll validate these data in the context of a unit test where
no output means each is
valid:

``` r
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster6), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster7), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster8), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster9), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster10), TRUE)
```

## Create Cluster Objects

With all data validated, we can now convert each to cluster object. This
process adds relevant metadata about each
cluster:

``` r
cluster1 <- qm_create(ref = grids, key = GRID_ID, value = cluster1, rid = params$pid, cid = 1, category = "frequent")
cluster2 <- qm_create(ref = grids, key = GRID_ID, value = cluster2, rid = params$pid, cid = 2, category = "frequent")
cluster3 <- qm_create(ref = grids, key = GRID_ID, value = cluster3, rid = params$pid, cid = 3, category = "frequent")
cluster4 <- qm_create(ref = grids, key = GRID_ID, value = cluster4, rid = params$pid, cid = 4, category = "frequent")
cluster5 <- qm_create(ref = grids, key = GRID_ID, value = cluster5, rid = params$pid, cid = 5, category = "frequent")
cluster6 <- qm_create(ref = grids, key = GRID_ID, value = cluster6, rid = params$pid, cid = 6, category = "regular")
cluster7 <- qm_create(ref = grids, key = GRID_ID, value = cluster7, rid = params$pid, cid = 7, category = "regular")
cluster8 <- qm_create(ref = grids, key = GRID_ID, value = cluster8, rid = params$pid, cid = 8, category = "regular")
cluster9 <- qm_create(ref = grids, key = GRID_ID, value = cluster9, rid = params$pid, cid = 9, category = "regular")
cluster10 <- qm_create(ref = grids, key = GRID_ID, value = cluster10, rid = params$pid, cid = 10, category = "regular")
```

## Combine Cluster Objects

With all of the cluster objects created, we can now combine them into a
single
object:

``` r
clusters <- qm_combine(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6,
                       cluster7, cluster8, cluster9, cluster10)
```

## Write Combined Object

Finally, we’ll write these data to a `.csv`
file:

``` r
write_csv(clusters, here("data", "participants", paste0("participant_", params$pid, ".csv")))
```
