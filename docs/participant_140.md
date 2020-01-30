Participant 140
================
Christopher Prener, Ph.D.
(January 30, 2020)

## Introduction

This notebook is used for digitizing participant data for the `qualmap`
pilot project.

## Dependencies

This notebook requires the following packages:

``` r
# spatial packages
library(qualmap)
library(sf)

# tidyverse packages
library(readr)

# other packages
library(here)
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
cluster2 <- qm_define("F-3", "G-3")
cluster3 <- qm_define("F-3", "G-3")
```

Next, we’ll validate these data. The validation is performed in the
context of a unit test to ensure that each object is
valid:

``` r
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster1), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster2), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster3), TRUE)
```

No output on these unit tests means that each cluster is indeed valid.

Next, we’ll repeat the process with the “regular” prompt by first
entering the cluster data:

``` r
cluster4 <- qm_define("F-3", "F-4", "G-4")
cluster5 <- qm_define("G-3", "H-3", "G-4", "H-4")
```

As before, we’ll validate these data in the context of a unit test where
no output means each is
valid:

``` r
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster4), TRUE)
expect_equal(qm_validate(ref = grids, key = GRID_ID, value = cluster5), TRUE)
```

## Create Cluster Objects

With all data validated, we can now convert each to cluster object. This
process adds relevant metadata about each
cluster:

``` r
cluster1 <- qm_create(ref = grids, key = GRID_ID, value = cluster1, rid = params$pid, cid = 1, category = "frequent")
cluster2 <- qm_create(ref = grids, key = GRID_ID, value = cluster2, rid = params$pid, cid = 2, category = "frequent")
cluster3 <- qm_create(ref = grids, key = GRID_ID, value = cluster3, rid = params$pid, cid = 3, category = "frequent")
cluster4 <- qm_create(ref = grids, key = GRID_ID, value = cluster4, rid = params$pid, cid = 4, category = "regular")
cluster5 <- qm_create(ref = grids, key = GRID_ID, value = cluster5, rid = params$pid, cid = 5, category = "regular")
```

## Combine Cluster Objects

With all of the cluster objects created, we can now combine them into a
single object:

``` r
clusters <- qm_combine(cluster1, cluster2, cluster3, cluster4, cluster5)
```

## Write Combined Object

Finally, we’ll write these data to a `.csv`
file:

``` r
write_csv(clusters, here("data", "participants", paste0("participant_", params$pid, ".csv")))
```
