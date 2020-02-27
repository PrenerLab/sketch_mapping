Create Crime Data
================
Christopher Prener, Ph.D.
(February 27, 2020)

## Introduction

This notebook downloads and validates raw crime data from the City of
St. Louis before calculating crime rates for the study area.

## Dependencies

This notebook requires the following packages:

``` r
# spatial packages
library(areal)
library(compstatr)
library(gateway)
library(postmastr)
library(sf)
```

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

``` r
library(tidycensus)
library(tigris)
```

    ## To enable 
    ## caching of data, set `options(tigris_use_cache = TRUE)` in your R script or .Rprofile.

    ## 
    ## Attaching package: 'tigris'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     plot

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
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(readr)
library(stringr)
library(tidyr)

# other packages
library(here)
```

    ## here() starts at /Users/prenercg/GitHub/PrenerLab/sketch_mapping

    ## 
    ## Attaching package: 'here'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     here

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

## Load Data

### Grid

We’ll load the reference data from
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

Not all grid squares were shown on the instrument, so we’ll remove some
of the outer grid squares:

``` r
grids %>% 
  separate(GRID_ID, into = c("col", "row"), sep = "-", remove = FALSE) %>%
  filter(col != "A") %>%
  filter(col != "I") %>%
  select(GRID_ID) -> grids
```

Finally, we’ll re-project the grid squares:

``` r
grids <- st_transform(grids, crs = 26915)
```

### Crime Data

To create our crime data set, we’ll download all 2020, 2019, and 2018
crimes and then validate them. First, we need to create an index and
then download the raw data:

``` r
# CREATE INDEX
i <- cs_create_index()

# DOWNLOAD DATA
## 2018
data2018_raw <- cs_get_data(year = 2018, index = i)

## 2019
data2019_raw <- cs_get_data(year = 2019, index = i)

## 2020
data2020_raw <- cs_get_data(year = 2020, index = i)
```

With the data downloaded, we’ll validate 2020:

``` r
### validate
expect_equal(cs_validate(data2020_raw, year = "2020"), TRUE)
expect_equal(length(data2020_raw), 1)

### collapse into single object
data2020_raw <- cs_collapse(data2020_raw)
```

We’ll also validate 2019, and create an object containing all 2019
crimes:

``` r
### validate
expect_equal(cs_validate(data2019_raw, year = "2019"), TRUE)
expect_equal(length(data2019_raw), 12)

### collapse into single object
data2019_raw <- cs_collapse(data2019_raw)

### combine and filter
cs_combine(type = "year", date = 2019, data2020_raw, data2019_raw) %>%
  cs_filter_count(var = count) -> crime2019
```

And finally we’ll validate 2018, and similarly create an object
containing all 2018 crimes:

``` r
### validate
expect_equal(cs_validate(data2018_raw, year = "2018"), TRUE)
expect_equal(length(data2018_raw), 12)

### collapse into single object
data2018_raw <- cs_collapse(data2018_raw)

### combine and filter
cs_combine(type = "year", date = 2018, data2020_raw, data2019_raw, data2018_raw) %>%
  cs_filter_count(var = count) -> crime2018
```

With our initial objects created, we’ll quickly clean-up our global
enviornment:

``` r
rm(data2019_raw, data2018_raw, data2020_raw, i)
```

For the purposes of this paper, we are only interested in Part 1 Crimes
that occured between May 1st, 2018 and April 30th, 2019. We’ll therefore
remove all other crimes, and then remove those remaining crimes that are
out of the relevant date range. With both subsetting tasks done, we can
combine the remaining crimes into a single data set:

``` r
## create part 1 objects
part1_2018 <- cs_filter_crime(crime2018, var = crime, crime = "Part 1")
part1_2019 <- cs_filter_crime(crime2019, var = crime, crime = "Part 1")

## subset by time
part1_2018 %>% 
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time)  %>%
  filter(date >= ymd("2018-04-01")) -> part1_2018

part1_2019 %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time)  %>%
  filter(date < ymd("2019-05-01")) -> part1_2019

## combine
crime <- bind_rows(part1_2018, part1_2019)
```

At this stage, we can remove all of the underlying data used to create
the `crime` object:

``` r
rm(part1_2018, part1_2019, crime2018, crime2019)
```

We’ll also remove quite a few columns that are no longer needed in
`crime`:

``` r
crime <- select(crime, -coded_month, -flag_crime, -flag_unfounded, -flag_administrative,
                     -count, -flag_cleanup, -district, -location_name, -location_comment,
                     -cad_address, -cad_street, -dateTime, -date, -time, -cs_year)
```

## City Boundary

We’ll also create a `sf` object

``` r
stl <- counties(state = 29, class = "sf") %>%
  st_transform(crs = 26915) %>%
  filter(COUNTYFP == 510) %>%
  mutate(STL = TRUE) %>%
  select(STL)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |=======================                                               |  34%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |==============================                                        |  44%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |==========================================================            |  84%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |=================================================================     |  94%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

## Improve Sample Size

There is a small degree of missing spatial data in these data:

``` r
### identify missing
crime %>% 
  cs_missingXY(varX = x_coord, varY = y_coord, newVar = missingXY) %>%
  mutate(missingXY = as.logical(missingXY)) -> crime

### store count of rows
count_crime <- nrow(crime)

### store count of rows missing spatial data
count_miss_crime <- sum(crime$missingXY, na.rm = TRUE)

## identify percent missing
(count_miss_crime/count_crime)*100
```

    ## [1] 2.167111

In order to address these missing data, we’ll parse the addresses for
those that are missing and attempt to geocode them. First, we need to
build a set of local geocoders:

``` r
geocoder <- gw_build_geocoder(style = "full", return = "coords")
geocoder_s <- gw_build_geocoder(style = "short", return = "coords")
geocoder_p <- gw_build_geocoder(style = "placename", return = "coords")
```

Then, we need to parse and geocode the data:

``` r
### subset missing
crime %>% 
  filter(missingXY == TRUE) %>%
  mutate(address = paste(ileads_address, ileads_street)) %>%
  mutate(address = str_replace(address, pattern = "[/@]", replacement = "at")) %>%
  mutate(address = ifelse(word(address, start = 1) == "0", 
                          word(address, start = 2, end = -1), address)) %>%
  select(-x_coord, -y_coord) -> crime_miss

### identify address types
crime_miss <- pm_identify(crime_miss, var = address)

### normalize
crime_miss <- pm_parse(crime_miss, input = "short", address = address, 
                            output = "short", new_address = address_clean, 
                            houseSuf_dict = stl_std_houseSuffix,
                            dir_dict = stl_std_directions,
                            street_dict = stl_std_streets,
                            suffix_dict = stl_std_suffix)
```

    ## Warning: `cols` is now required.
    ## Please use `cols = c(pm.address)`

    ## Warning: `cols` is now required.
    ## Please use `cols = c(data, y)`

``` r
### subset based on cleaned address
crime_miss_attmpt <- filter(crime_miss, address_clean %in% c("Na Na", "Na Unknown", "Unknown", "Uknown") == FALSE)
crime_miss_unknwn <- filter(crime_miss, address_clean %in% c("Na Na", "Na Unknown", "Unknown", "Uknown") == TRUE)

### geocode
crime_miss_attmpt <- gw_geocode(crime_miss_attmpt, type = "composite, full", 
                                var = address_clean, class = "tibble", 
                                local = geocoder, local_short = geocoder_s, 
                                local_place = geocoder_p, threshold = 90)
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 56 rows [1, 2, 3,
    ## 4, 6, 7, 8, 9, 10, 11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, ...].

``` r
### reformat geocode data
crime_miss_attmpt %>%
  rename(x = gw_x, y = gw_y) %>%
  mutate(gw_address = ifelse(is.na(gw_source) == FALSE & is.na(gw_address) == TRUE, address_clean, gw_address)) %>%
  select(-missingXY, -address, -gw_addrrecnum, -gw_id, -address_clean) -> crime_miss_attmpt

### subset valid
crime <- filter(crime, missingXY == FALSE)

### clean-up valid
crime %>% 
  select(-missingXY) %>%
  cs_projectXY(varX = x_coord, varY = y_coord, crs = 4269) %>%
  gw_get_coords() -> crime

st_geometry(crime) <- NULL

### combine valid and geocoded data
crime_miss <- bind_rows(crime_miss_attmpt, crime_miss_unknwn)
crime <- bind_rows(crime, crime_miss)

### unit test combined data
expect_equal(nrow(crime), count_crime)
```

With our geocoding complete, we’ll identify still missing spatial data:

``` r
# identify missing
crime %>% 
  cs_missingXY(varX = x, varY = y, newVar = missingXY) %>%
  mutate(missingXY = as.logical(missingXY)) -> crime

### store count of still-missing spatial data
count_miss2_crime <- sum(crime$missingXY, na.rm = TRUE)

## identify percent missing
(count_miss2_crime/count_crime)*100
```

    ## [1] 0.6567003

We’ve improved from 2.2% missing to .66% missing. Next, we’ll remove
those crimes that occured outside of the city:

``` r
### project
crime %>%
  filter(missingXY == FALSE) %>%
  select(-missingXY) %>%
  st_as_sf(coords = c("x", "y"), crs = 4269) %>%
  st_transform(crs = 26915) -> crime

### unit test
expect_equal(nrow(crime), count_crime-count_miss2_crime)

### store count of data prior to intersect
count_pre_city <- nrow(crime)

### intersect
crime %>%
  st_intersection(., stl) %>%
  select(-STL) -> crime
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
### store count of data in city
count_city <- nrow(crime)
```

With a final data set of crimes for this time period constructed, we’ll
clean-up our enviornment
again:

``` r
rm(geocoder, geocoder_p, geocoder_s, stl, crime_miss, crime_miss_attmpt, crime_miss_unknwn)
```

## Count Crimes in Grid Squares

Next, we want to join crimes to grid squares and create a count of
crimes per grid square. We’ll create three counts, one for all Part 1
crimes, one for violent crimes, and one for property crimes:

``` r
## join
crime %>%
  st_intersection(., grids) %>%
  select(GRID_ID, crime) -> crime_grid
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
## remove spatial attribute
st_geometry(crime_grid) <- NULL

## store remaining n
count_grid <- nrow(crime_grid)

## subset
crime_grid_violent <- cs_filter_crime(crime_grid, var = crime, crime = "Violent")
crime_grid_property <- cs_filter_crime(crime_grid, var = crime, crime = "Property")

## unit test
expect_equal(count_grid, nrow(crime_grid_property)+nrow(crime_grid_violent))

## create counts per grid square
### part 1 crimes
crime_grid %>%
  group_by(GRID_ID) %>%
  summarise(part1 = n()) -> crime_grid_p1

### violent crimes
crime_grid_violent %>%
  group_by(GRID_ID) %>%
  summarise(violent = n()) -> crime_grid_violent

### violent crimes
crime_grid_property %>%
  group_by(GRID_ID) %>%
  summarise(property = n()) -> crime_grid_property

## combine
left_join(grids, crime_grid_p1, by = "GRID_ID") %>%
  left_join(., crime_grid_violent, by = "GRID_ID") %>%
  left_join(., crime_grid_property, by = "GRID_ID") -> crime_grid
```

Finally, we can clean-up our enviornment again:

``` r
rm(crime, crime_grid_p1, crime_grid_property, crime_grid_violent)
```

## Store Record of How Data Evolved

To trace the changes in the larger sample of crime data, we’ll write our
counts to `missing_crime` and then save them in our `data/crime`
directory:

``` r
# create crime object
missing_crime <- tibble(
  orig = count_crime,
  miss_pre = count_miss_crime,
  miss_post = count_miss2_crime,
  pre_city = count_pre_city,
  city = count_city,
  grid = count_grid,
  grid_v = sum(crime_grid$violent, na.rm = TRUE),
  grid_p = sum(crime_grid$property, na.rm = TRUE)
)

# write data
write_csv(missing_crime, path = here("data", "crime", "missing_crime.csv"))

# clean-up enviornment
rm(count_crime, count_miss_crime, count_miss2_crime, count_pre_city, count_city, count_grid)
```

## Calculate Crime Rates

With our data geocoded, we can then calculate crime rates. We’ll need
tract level data for St. Louis to do this:

``` r
## download tract data
pop <- get_acs(year = 2018, geography = "tract", variables = "B01003_001", 
               state = 29, county = 510, geometry = TRUE) %>%
  st_transform(crs = 26915) %>%
  select(GEOID, estimate) %>%
  rename(total_pop = estimate)
```

    ## Getting data from the 2014-2018 5-year ACS

    ## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  72%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  75%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%

With the tract level population estimates, we can interpoalte them to
grid squares using the `areal`
package:

``` r
grids <- aw_interpolate(grids, tid = GRID_ID, source = pop, sid = GEOID, 
                        weight = "total", output = "tibble", extensive = "total_pop")
```

Once they are interpoalted, we can calculate rates per 1,000 estimated
residents:

``` r
crime_grid %>%
  left_join(., grids, by = "GRID_ID") %>%
  mutate(
    part1_rate = part1/total_pop*1000,
    violent_rate = violent/total_pop*1000,
    property_rate = property/total_pop*1000
  ) %>%
  select(GRID_ID, total_pop, part1, part1_rate, violent, violent_rate, property, property_rate) -> crime_grid
```

These data can then be written to `data/crime`:

``` r
# write data
st_write(crime_grid, dsn = here("data", "crime", "crime_grid", "crime_grid.shp"), delete_dsn = TRUE)
```

    ## Warning in abbreviate_shapefile_names(obj): Field names abbreviated for ESRI
    ## Shapefile driver

    ## Deleting source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/crime/crime_grid/crime_grid.shp' using driver `ESRI Shapefile'
    ## Writing layer `crime_grid' to data source `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/crime/crime_grid/crime_grid.shp' using driver `ESRI Shapefile'
    ## Writing 49 features with 8 fields and geometry type Polygon.

``` r
# clean-up
rm(grids, pop)
```
