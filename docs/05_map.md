Map Clusters
================
Christopher Prener, Ph.D.
(November 01, 2021)

## Introduction

This notebook imports the shapefiles created by `02_summarize.Rmd` and
`03_crime.Rmd`, and then maps them. Correlation calculations are also
made at the end of the notebook.

## Dependencies

This notebook requires the following packages:

``` r
# spatial packages
library(sf)
```

    ## Linking to GEOS 3.8.1, GDAL 3.2.1, PROJ 7.2.1

``` r
library(tmap)

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
library(ggplot2)

# other packages
library(cowplot)
library(here)
```

    ## here() starts at /Users/prenercg/GitHub/PrenerLab/sketch_mapping

We also a custom function we’ll use:

``` r
corrTable <-function(.data, coef = c("pearson", "spearman"), listwise = TRUE, round = 3, pStar = TRUE, ...){
  
  ## process dots
  if (rlang::dots_n(...) > 1) {
    .data <- dplyr::select(.data, ...)
  }
  
  ## listwise deletion
  if (listwise == TRUE) {
    .data <- na.omit(.data)
  }
  
  ## compute correlation matrix
  inputMatrix <- as.matrix(.data)
  corrMatrix <- Hmisc::rcorr(inputMatrix, type = coef)
  
  ## matrix of correlation coeficients
  rCoef <- corrMatrix$r
  
  # matrix of p-values
  pValues <- corrMatrix$P  
  
  ## round the correlation matrix values
  rCoef <- format(round(cbind(rep(-1.11, ncol(.data)), rCoef), round))[,-1]
  
  ## statistical significance stars
  if (pStar == TRUE) {
    
    ## Define notions for significance levels
    stars <- ifelse(pValues < .001, "***", 
                    ifelse(pValues < .01, "**", 
                           ifelse(pValues < .05, "*", "")))
    
    ## add apropriate stars
    rCoef <- matrix(paste(rCoef, stars, sep = ""), ncol = ncol(.data))    
  }
  
  ## remove upper triangle
  rCoef[upper.tri(rCoef, diag = FALSE)] <- ""
  diag(rCoef) <- formatC(1, digits = round, format = "f")
  
  ## final formatting
  rCoef <- as.data.frame(rCoef)
  rCoef <- dplyr::mutate_if(rCoef, is.factor, as.character)
  rownames(rCoef) <- colnames(.data)
  colnames(rCoef) <- paste(colnames(.data), "", sep = "")

  
  ## return data frame
  return(rCoef)
} 
```

## Load Data

The following shapefiles with participant data are needed:

``` r
freq_clusters <- st_read(here("data", "summaries", "freq_clusters"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `freq_clusters' from data source 
    ##   `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/freq_clusters' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 49 features and 5 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 885839.8 ymin: 1011334 xmax: 900154.4 ymax: 1025649
    ## Projected CRS: Transverse_Mercator

``` r
freq_respondents <- st_read(here("data", "summaries", "freq_respondents"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `freq_respondents' from data source 
    ##   `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/freq_respondents' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 49 features and 5 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 885839.8 ymin: 1011334 xmax: 900154.4 ymax: 1025649
    ## Projected CRS: Transverse_Mercator

``` r
reg_clusters <- st_read(here("data", "summaries", "reg_clusters"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `reg_clusters' from data source 
    ##   `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/reg_clusters' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 49 features and 5 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 885839.8 ymin: 1011334 xmax: 900154.4 ymax: 1025649
    ## Projected CRS: Transverse_Mercator

``` r
reg_respondents <- st_read(here("data", "summaries", "reg_respondents"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `reg_respondents' from data source 
    ##   `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/summaries/reg_respondents' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 49 features and 5 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 885839.8 ymin: 1011334 xmax: 900154.4 ymax: 1025649
    ## Projected CRS: Transverse_Mercator

``` r
crime <- st_read(here("data", "crime", "crime_grid"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `crime_grid' from data source 
    ##   `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/crime/crime_grid' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 49 features and 8 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 737559.2 ymin: 4277125 xmax: 742041.3 ymax: 4281607
    ## Projected CRS: UTM_Zone_15_Northern_Hemisphere

These additional shapefiles are also used for mapping:

``` r
metro <- st_read(here("data", "spatial", "STL_TRANS_Metrolink"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `STL_TRANS_Metrolink' from data source 
    ##   `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/spatial/STL_TRANS_Metrolink' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 1 feature and 6 fields
    ## Geometry type: MULTILINESTRING
    ## Dimension:     XY
    ## Bounding box:  xmin: -90.31973 ymin: 38.5928 xmax: -90.17888 ymax: 38.66169
    ## Geodetic CRS:  WGS 84

``` r
roads <- st_read(here("data", "spatial", "STL_TRANS_PrimaryRoads"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `STL_TRANS_PrimaryRoads' from data source 
    ##   `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/spatial/STL_TRANS_PrimaryRoads' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 89 features and 5 fields
    ## Geometry type: LINESTRING
    ## Dimension:     XY
    ## Bounding box:  xmin: -90.31897 ymin: 38.53756 xmax: -90.17253 ymax: 38.76759
    ## Geodetic CRS:  NAD83

``` r
campus <- st_read(here("data", "spatial", "ZONE_CAMPUS_SLU"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)
```

    ## Reading layer `ZONE_CAMPUS_SLU' from data source 
    ##   `/Users/prenercg/GitHub/PrenerLab/sketch_mapping/data/spatial/ZONE_CAMPUS_SLU' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 4 features and 2 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -10045680 ymin: 4666920 xmax: -10043860 ymax: 4670081
    ## Projected CRS: WGS 84 / Pseudo-Mercator

## Geoprocessing Data

There is some geoprocessing we need to do. First, we need to crop some
our shapefiles that provide cartographic data. We’ll create a study area
object:

``` r
freq_clusters %>%
  mutate(id = 1) %>%
  group_by(id) %>%
  summarise(name = "study area") -> study_area
```

We can use the `study_area` object to crop roads:

``` r
roads <- st_crop(roads, study_area) %>%
  filter(RTTYP == "I")
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

And the metro lines:

``` r
metro <- st_crop(metro, study_area)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

We don’t have a need for the `study_area` object after this:

``` r
rm(study_area)
```

## Re-Project

With our data processed, we’ll re-project them to NAD 1983 so our map is
not slightly tilted:

``` r
freq_clusters <- st_transform(freq_clusters, crs = 4269)
freq_respondents <- st_transform(freq_respondents, crs = 4269)
reg_respondents <- st_transform(reg_respondents, crs = 4269)
reg_clusters <- st_transform(reg_clusters, crs = 4269)

crime <- st_transform(crime, crs = 4269)

roads <- st_transform(roads, crs = 4269)
campus <- st_transform(campus, crs = 4269)
metro <- st_transform(metro, crs = 4269)
```

## Create Labels

First, we’ll create map labels for SLU’s campus:

``` r
# define points
labels <- tibble(
  name = c("North Campus", "South Campus"),
  x = c(-90.235964, -90.236005),
  y = c(38.639756, 38.620040)
)

# project points
labels <- st_as_sf(labels, coords = c("x", "y"), crs = 4269)
```

Next, for neighborhoods:

``` r
# define points
nhood <- tibble(
  name = c("Central West End", "Grand Center", "Forest Park Southeast"),
  x = c(-90.255050, -90.232176, -90.256972),
  y = c(38.641675, 38.642287, 38.626802)
)

# project points
nhood <- st_as_sf(nhood, coords = c("x", "y"), crs = 4269)
```

Next, for interstate highways and the metro line:

``` r
# define points
features <- tibble(
  name = c("I-44", "I-64", "Metro"),
  x = c(-90.268058, -90.268058, -90.265650),
  y = c(38.616898, 38.628251, 38.642927)
)

# project points
features <- st_as_sf(features, coords = c("x", "y"), crs = 4269)
```

## Frequent Clusters

### Map Counts of Clusters

``` r
# create map object
tm_shape(freq_clusters) +
    tm_polygons(title = "Count of Clusters", col = "frequent", style = "jenks") +
  tm_shape(roads) +
    tm_lines(lwd = 1.5, col = "#474747") +
  tm_shape(metro) +
    tm_lines() +
  tm_shape(campus) +
    tm_borders(lwd = 2, col = "#000000") +
  tm_shape(labels) +
    tm_text("name", fontface = 2, just = "left", size = 1, shadow = TRUE) +
  tm_shape(nhood) +
    tm_text("name", fontface = 4, size = .75, shadow = TRUE) +
  tm_shape(features) +
    tm_text("name", fontface = 3, just = "bottom", size = .75, shadow = TRUE) +
  tm_layout(title = "Frequent Visits \nby Cluster",
            frame = FALSE, outer.margins = c(0, 0, 0, 0)) +
  tm_scale_bar(position=c("right", "bottom"), just = "top") +
  tm_legend(outside = TRUE, attr.outside = TRUE) -> freq_clusters_map

# save map
tmap_save(tm = freq_clusters_map, filename = here("results", "freq_clusters_map.png"), dpi = 500)
```

    ## Map saved to /Users/prenercg/GitHub/PrenerLab/sketch_mapping/results/freq_clusters_map.png

    ## Resolution: 3492.825 by 3507.19 pixels

    ## Size: 6.98565 by 7.014379 inches (500 dpi)

``` r
# remove map
rm(freq_clusters_map)
```

### Percent of Frequent Clusters

``` r
# prep data
freq_respondents <- mutate(freq_respondents, frequent_pct = frequent/53*100)
breaks <- c(0, 8, 16.5, 33, 66, 100)

# create map object
tm_shape(freq_respondents) +
    tm_polygons(title = "% of Respondents", col = "frequent_pct", 
                breaks = breaks, palette = "Blues") +
  tm_shape(roads) +
    tm_lines(lwd = 1.5, col = "#474747") +
  tm_shape(metro) +
    tm_lines() +
  tm_shape(campus) +
    tm_borders(lwd = 2, col = "#000000") +
  tm_shape(labels) +
    tm_text("name", fontface = 2, just = "left", size = 1, shadow = TRUE) +
  tm_shape(nhood) +
    tm_text("name", fontface = 4, size = .75, shadow = TRUE) +
  tm_shape(features) +
    tm_text("name", fontface = 3, just = "bottom", size = .75, shadow = TRUE) +
  tm_layout(title = "Frequent Visits \nby Respondent",
            frame = FALSE, outer.margins = c(0,0,0,0)) +
  tm_scale_bar(position=c("right", "bottom"), just = "top") +
  tm_legend(outside = TRUE, attr.outside = TRUE) -> freq_respondents_map

# save map
tmap_save(tm = freq_respondents_map, filename = here("results", "freq_respondents_map.png"), dpi = 500)
```

    ## Map saved to /Users/prenercg/GitHub/PrenerLab/sketch_mapping/results/freq_respondents_map.png

    ## Resolution: 3492.825 by 3507.19 pixels

    ## Size: 6.98565 by 7.014379 inches (500 dpi)

``` r
# remove map
rm(freq_respondents_map)
```

### Publication Map

``` r
# create map object
tm_shape(freq_respondents) +
    tm_polygons(title = "% of Respondents", col = "frequent_pct", 
                breaks = breaks, palette = "Blues") +
  tm_shape(roads) +
    tm_lines(lwd = 1.5, col = "#474747") +
  tm_shape(metro) +
    tm_lines() +
  tm_shape(campus) +
    tm_borders(lwd = 2, col = "#000000") +
  tm_shape(labels) +
    tm_text("name", fontface = 2, just = "left", size = 1, shadow = TRUE) +
  tm_shape(nhood) +
    tm_text("name", fontface = 4, size = .75, shadow = TRUE) +
  tm_shape(features) +
    tm_text("name", fontface = 3, just = "bottom", size = .75, shadow = TRUE) +
  tm_layout(frame = FALSE, outer.margins = c(0,0,0,0)) +
  tm_scale_bar(position=c("right", "bottom"), just = "top") +
  tm_legend(outside = TRUE, attr.outside = TRUE) -> freq_respondents_map

# save map
tmap_save(tm = freq_respondents_map, filename = here("results", "figure3.pdf"), dpi = 600)
```

    ## Map saved to /Users/prenercg/GitHub/PrenerLab/sketch_mapping/results/figure3.pdf

    ## Size: 6.972222 by 7.013889 inches

``` r
# remove map
rm(freq_respondents_map)
```

## Regular Clusters

### Map Counts of Clusters

``` r
# create map object
tm_shape(reg_clusters) +
    tm_polygons(title = "Count of Clusters", col = "regular", 
                style = "jenks", palette = "Reds") +
  tm_shape(roads) +
    tm_lines(lwd = 1.5, col = "#474747") +
  tm_shape(metro) +
    tm_lines() +
  tm_shape(campus) +
    tm_borders(lwd = 2, col = "#000000") +
  tm_shape(labels) +
    tm_text("name", fontface = 2, just = "left", size = 1, shadow = TRUE) +
  tm_shape(nhood) +
    tm_text("name", fontface = 4, size = .75, shadow = TRUE) +
  tm_shape(features) +
    tm_text("name", fontface = 3, just = "bottom", size = .75, shadow = TRUE) +
  tm_layout(title = "Regular Visits \nby Cluster",
            frame = FALSE, outer.margins = c(0,0,0,0)) +
  tm_scale_bar(position=c("right", "bottom"), just = "top") +
  tm_legend(outside = TRUE, attr.outside = TRUE) -> reg_clusters_map

# save map
tmap_save(tm = reg_clusters_map, filename = here("results", "reg_clusters_map.png"), dpi = 500)
```

    ## Map saved to /Users/prenercg/GitHub/PrenerLab/sketch_mapping/results/reg_clusters_map.png

    ## Resolution: 3492.825 by 3507.19 pixels

    ## Size: 6.98565 by 7.014379 inches (500 dpi)

``` r
# remove map
rm(reg_clusters_map)
```

### Percent of Regular Clusters

``` r
# prep data
reg_respondents <- mutate(reg_respondents, regular_pct = regular/53*100)
breaks <- c(0, 8, 16.5, 33, 49.5, 66)

tm_shape(reg_respondents) +
    tm_polygons(title = "% of Respondents", col = "regular_pct", 
                breaks = breaks, palette = "Greens") +
  tm_shape(roads) +
    tm_lines(lwd = 1.5, col = "#474747") +
  tm_shape(metro) +
    tm_lines() +
  tm_shape(campus) +
    tm_borders(lwd = 2, col = "#000000") +
  tm_shape(labels) +
    tm_text("name", fontface = 2, just = "left", size = 1, shadow = TRUE) +
  tm_shape(nhood) +
    tm_text("name", fontface = 4, size = .75, shadow = TRUE) +
  tm_shape(features) +
    tm_text("name", fontface = 3, just = "bottom", size = .75, shadow = TRUE) +
  tm_layout(title = "Regular Visits \nby % of Respondents",
            frame = FALSE, outer.margins = c(0,0,0,0)) +
  tm_scale_bar(position=c("right", "bottom"), just = "top") +
  tm_legend(outside = TRUE, attr.outside = TRUE) -> reg_respondents_map

# save map
tmap_save(tm = reg_respondents_map, filename = here("results", "reg_respondents_map.png"), dpi = 500)
```

    ## Map saved to /Users/prenercg/GitHub/PrenerLab/sketch_mapping/results/reg_respondents_map.png

    ## Resolution: 3492.825 by 3507.19 pixels

    ## Size: 6.98565 by 7.014379 inches (500 dpi)

``` r
# remove map
rm(reg_respondents_map)
```

### Publication Map

``` r
# create map object
tm_shape(reg_respondents) +
    tm_polygons(title = "% of Respondents", col = "regular_pct", 
                breaks = breaks, palette = "Greens") +
  tm_shape(roads) +
    tm_lines(lwd = 1.5, col = "#474747") +
  tm_shape(metro) +
    tm_lines() +
  tm_shape(campus) +
    tm_borders(lwd = 2, col = "#000000") +
  tm_shape(labels) +
    tm_text("name", fontface = 2, just = "left", size = 1, shadow = TRUE) +
  tm_shape(nhood) +
    tm_text("name", fontface = 4, size = .75, shadow = TRUE) +
  tm_shape(features) +
    tm_text("name", fontface = 3, just = "bottom", size = .75, shadow = TRUE) +
  tm_layout(frame = FALSE, outer.margins = c(0,0,0,0)) +
  tm_scale_bar(position=c("right", "bottom"), just = "top") +
  tm_legend(outside = TRUE, attr.outside = TRUE) -> reg_respondents_map

# save map
tmap_save(tm = reg_respondents_map, filename = here("results", "figure4.pdf"), dpi = 600)
```

    ## Map saved to /Users/prenercg/GitHub/PrenerLab/sketch_mapping/results/figure4.pdf

    ## Size: 6.972222 by 7.013889 inches

``` r
# remove map
rm(reg_respondents_map)
```

## Crime Maps

### Property Crime Map

``` r
# create map object
tm_shape(crime) +
    tm_polygons(title = "Property Crime Rates\nper 1,000 Estimated Residents", col = "prprty_", 
                style = "jenks", palette = "BuGn") +
  tm_shape(roads) +
    tm_lines(lwd = 1.5, col = "#474747") +
  tm_shape(metro) +
    tm_lines() +
  tm_shape(campus) +
    tm_borders(lwd = 2, col = "#000000") +
  tm_shape(labels) +
    tm_text("name", fontface = 2, just = "left", size = 1, shadow = TRUE) +
  tm_shape(nhood) +
    tm_text("name", fontface = 4, size = .75, shadow = TRUE) +
  tm_shape(features) +
    tm_text("name", fontface = 3, just = "bottom", size = .75, shadow = TRUE) +
  tm_layout(frame = FALSE, outer.margins = c(0,0,0,0)) +
  tm_scale_bar(position=c("right", "bottom"), just = "top") +
  tm_legend(outside = TRUE, attr.outside = TRUE) -> property_map

# save map
tmap_save(tm = property_map, filename = here("results", "property_map.png"), dpi = 500)
```

    ## Map saved to /Users/prenercg/GitHub/PrenerLab/sketch_mapping/results/property_map.png

    ## Resolution: 3492.825 by 3507.19 pixels

    ## Size: 6.98565 by 7.014379 inches (500 dpi)

``` r
# remove map
rm(property_map)
```

### Violent Crime Map

``` r
# create map object
tm_shape(crime) +
    tm_polygons(title = "Violent Crime Rates\nper 1,000 Estimated Residents", col = "vlnt_rt", 
                style = "jenks", palette = "BuGn") +
  tm_shape(roads) +
    tm_lines(lwd = 1.5, col = "#474747") +
  tm_shape(metro) +
    tm_lines() +
  tm_shape(campus) +
    tm_borders(lwd = 2, col = "#000000") +
  tm_shape(labels) +
    tm_text("name", fontface = 2, just = "left", size = 1, shadow = TRUE) +
  tm_shape(nhood) +
    tm_text("name", fontface = 4, size = .75, shadow = TRUE) +
  tm_shape(features) +
    tm_text("name", fontface = 3, just = "bottom", size = .75, shadow = TRUE) +
  tm_layout(frame = FALSE, outer.margins = c(0,0,0,0)) +
  tm_scale_bar(position=c("right", "bottom"), just = "top") +
  tm_legend(outside = TRUE, attr.outside = TRUE) -> violent_map

# save map
tmap_save(tm = violent_map, filename = here("results", "violent_map.png"), dpi = 500)
```

    ## Map saved to /Users/prenercg/GitHub/PrenerLab/sketch_mapping/results/violent_map.png

    ## Resolution: 3492.825 by 3507.19 pixels

    ## Size: 6.98565 by 7.014379 inches (500 dpi)

``` r
# remove map
rm(violent_map)
```

### Clean-up Map Objects

We can now get rid of several cartographic data objects:

``` r
rm(campus, features, freq_clusters, labels, metro, nhood, reg_clusters, roads, breaks)
```

## Crime Analysis

We’ll also caluclate correlation statistics comparing the respondent
data to crime rates.

### Prepare Data

First, we need to convert them back to data frames from `sf` objects:

``` r
st_geometry(freq_respondents) <- NULL
st_geometry(reg_respondents) <- NULL
st_geometry(crime) <- NULL
```

Then, we’ll join our data together:

``` r
# joins
crime_freq <- left_join(crime, freq_respondents, by = "GRID_ID")
crime_reg <- left_join(crime, reg_respondents, by = "GRID_ID")

# clean-up
rm(crime, freq_respondents, reg_respondents)
```

### Plot Data

First, we’ll plot the frequent data as a scatterplot with Part 1 crime
rates:

``` r
ggplot(data = crime_freq, mapping = aes(prt1_rt, frequent_pct/100)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Frequent Visits and Crime",
    x = "Part 1 Crime Rate (crimes per 1,000 estimated residents)",
    y = "% of Respondents with Frequent Visits"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1, by = .2)) +
  scale_x_continuous(breaks = seq(0,250, by = 25), limits = c(0,250)) -> a
```

We’ll make the same plot, but for the regular data:

``` r
ggplot(data = crime_reg, mapping = aes(prt1_rt, regular_pct/100)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Regular Visits and Crime",
    x = "Part 1 Crime Rate (crimes per 1,000 estimated residents)",
    y = "% of Respondents with Regular Visits"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1, by = .2)) +
  scale_x_continuous(breaks = seq(0,250, by = 25), limits = c(0,250)) -> b
```

Then, we’ll stitch both together using the `cowplot` package:

``` r
# create plot grid
c <- plot_grid(a, b, labels = c('A', 'B'), ncol = 1, align = "v")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

``` r
# save plot
ggsave(filename = here("results", "figure6.pdf"), dpi = 600, units = "in", width = 8, height = 8 )

# clean-up
rm(a, b, c)
```

## Correlations

Finally, we’ll calculate correlation coefficients and p-values. First,
the frequent data:

``` r
crime_freq %>%
  select(prt1_rt, frequent_pct) %>%
  corrTable(coef = "pearson")
```

    ##              prt1_rt frequent_pct
    ## prt1_rt        1.000             
    ## frequent_pct   0.029        1.000

And finally, the regular data:

``` r
crime_reg %>%
  select(prt1_rt, regular_pct) %>%
  corrTable(coef = "pearson")
```

    ##             prt1_rt regular_pct
    ## prt1_rt       1.000            
    ## regular_pct   0.096       1.000

## Clean-up

We can now remove all objects:

``` r
rm(corrTable, crime_freq, crime_reg)
```
