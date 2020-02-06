Summarize Clusters
================
Christopher Prener, Ph.D.
(February 06, 2020)

## Introduction

This notebook imports all of the raw cluster data and summarizes it into
mappable `sf` objects.

## Dependencies

This notebook requires the following packages:

``` r
# spatial packages
library(qualmap)
library(sf)
```

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

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
library(purrr)
library(readr)
library(tidyr)

# other packages
library(here)
```

    ## here() starts at /Users/prenercg/GitHub/PrenerLab/sketch_mapping

## Load Data

First, we need to create a list of the `.csv` files to be read in:

``` r
files <- dir(path = here("data", "participants"), pattern = ".csv")
```

Next, we can read them in and reduce them to a single data frame with
`purrr`:

``` r
data <- files %>%
  map_df(~read_csv(file = here("data", "participants", .x)))
```

    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   RID = col_double(),
    ##   CID = col_double(),
    ##   CAT = col_character(),
    ##   GRID_ID = col_character()
    ## )

Finally, we’ll load the reference data from
    `data/grids/`:

``` r
grids <- st_read(here("data", "grid"), stringsAsFactors = FALSE)
```

    ## Reading layer `grid' from data source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/grid' using driver `ESRI Shapefile'
    ## Simple feature collection with 63 features and 2 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 883794.9 ymin: 1011334 xmax: 902199.3 ymax: 1025649
    ## epsg (SRID):    102696
    ## proj4string:    +proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs

## Modify Grid

Not all grid squares were shown on the instrument, so we’ll remove some
of the outer grid squares:

``` r
grids %>% 
  separate(GRID_ID, into = c("col", "row"), sep = "-", remove = FALSE) %>%
  filter(col != "A") %>%
  filter(col != "I") -> grids
```

## Verify

Next, we can use the `qualmap` function `qm_verify()` to ensure we have
data that are appropriate for summarization:

``` r
data <- qm_verify(data)
```

`qm_verify` will error if there is an issue. Otherwise, it returns a
tibble with the class `"qm_cluster"` applied to it, which is necessary
for using `qm_summarize()` below.

## Summarize

The `qualmap` package comes with two options for counting, by cluster
and respondent. Theoretically, a feature could be included in multiple
clusters by a single respondent. Setting `count = "clusters"` will
return a count of the number of clusters that a feature is included in,
whereas `count = "respondents"` returns a count of the number of
respondents who identified that feature in at least one cluster.

We’ll create output using both options for the “frequent” qualitative
construct:

``` r
freq_clusters <- qm_summarize(ref = grids, key = "GRID_ID", clusters = data, category = "frequent", count = "clusters")
freq_respondents <- qm_summarize(ref = grids, key = "GRID_ID", clusters = data, category = "frequent", count = "respondents")
```

We’ll also create output using both options for the “regular”
qualitative
construct:

``` r
reg_clusters <- qm_summarize(ref = grids, key = "GRID_ID", clusters = data, category = "regular", count = "clusters")
reg_respondents <- qm_summarize(ref = grids, key = "GRID_ID", clusters = data, category = "regular", count = "respondents")
```

## Write Data

Finally, we’ll write each of the files to `data/` as
shapefiles:

``` r
st_write(freq_clusters, here("data", "summaries", "freq_clusters", "freq_clusters.shp"), delete_dsn = TRUE)
```

    ## Deleting source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/freq_clusters/freq_clusters.shp' using driver `ESRI Shapefile'
    ## Writing layer `freq_clusters' to data source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/freq_clusters/freq_clusters.shp' using driver `ESRI Shapefile'
    ## Writing 49 features with 5 fields and geometry type Polygon.

``` r
st_write(freq_respondents, here("data", "summaries", "freq_respondents", "freq_respondents.shp"), delete_dsn = TRUE)
```

    ## Deleting source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/freq_respondents/freq_respondents.shp' using driver `ESRI Shapefile'
    ## Writing layer `freq_respondents' to data source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/freq_respondents/freq_respondents.shp' using driver `ESRI Shapefile'
    ## Writing 49 features with 5 fields and geometry type Polygon.

``` r
st_write(reg_clusters, here("data", "summaries", "reg_clusters", "reg_clusters.shp"), delete_dsn = TRUE)
```

    ## Deleting source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/reg_clusters/reg_clusters.shp' using driver `ESRI Shapefile'
    ## Writing layer `reg_clusters' to data source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/reg_clusters/reg_clusters.shp' using driver `ESRI Shapefile'
    ## Writing 49 features with 5 fields and geometry type Polygon.

``` r
st_write(reg_respondents, here("data", "summaries", "reg_respondents", "reg_respondents.shp"), delete_dsn = TRUE)
```

    ## Deleting source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/reg_respondents/reg_respondents.shp' using driver `ESRI Shapefile'
    ## Writing layer `reg_respondents' to data source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/reg_respondents/reg_respondents.shp' using driver `ESRI Shapefile'
    ## Writing 49 features with 5 fields and geometry type Polygon.
