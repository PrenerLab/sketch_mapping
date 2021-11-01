Additional Maps
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

``` r
library(measurements)
library(stargazer)
```

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

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
    
  } else if (pStar == FALSE) {
    
    rCoef <- matrix(paste(rCoef, " (", round(pValues, digits = 3), ")", sep = ""), ncol = ncol(.data)) 
    
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
freq_respondents <- st_read(here("data", "summaries", "freq_respondents"), quiet = TRUE) %>%
  st_transform(crs = 26915)

reg_respondents <- st_read(here("data", "summaries", "reg_respondents"), quiet = TRUE) %>%
  st_transform(crs = 26915)
```

These additional shapefiles are also used for mapping:

``` r
zoning <- st_read(here("data", "spatial", "STL_PLACE_Zoning", "STL_PLACE_Zoning.shp"), quiet = TRUE) %>%
  st_transform(crs = 26915)

crime <- st_read(here("data", "crime", "crime_grid"), quiet = TRUE) %>%
  select(GRID_ID, totl_pp, part1, prt1_rt)

st_geometry(crime) <- NULL
```

## Geoprocessing Data

There is some geoprocessing we need to do. First, we need to crop some
our shapefiles that provide cartographic data. We’ll create a study area
object:

``` r
freq_respondents %>%
  mutate(id = 1) %>%
  group_by(id) %>%
  summarise(name = "study area") -> study_area
```

We can use the `study_area` object to crop our data:

``` r
zoning <- st_crop(zoning, study_area)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

With the data cropped, we’ll calculate the percentage of each grid
square occupied with commercial properties (which include University
areas):

``` r
zoning_per_grid <- filter(zoning, LAYER %in% c("G", "H", "I", "J"))

reg_respondents %>%
  select(GRID_ID) %>%
  st_intersection(zoning_per_grid, .) %>%
  group_by(GRID_ID) %>%
  summarise() %>%
  st_collection_extract(type = "POLYGON") %>%
  mutate(sq_m = as.numeric(st_area(geometry))) %>%
  mutate(sq_km = conv_unit(sq_m, from = "m2", to = "km2")) %>%
  mutate(pct_comm = sq_km*100) %>%
  select(GRID_ID, sq_km, pct_comm) -> zoning_per_grid
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

Now that we have that measure calculated, we’ll combine it with both the
frequent and regular visit data:

``` r
# remove geometry
st_geometry(zoning_per_grid) <- NULL

# combine data
freq_respondents %>% 
  left_join(., zoning_per_grid, by = "GRID_ID") %>%
  mutate(frequent_pct = frequent/53*100) %>%
  mutate(pct_comm = ifelse(is.na(pct_comm) == TRUE, 0, pct_comm)) -> freq_respondents

reg_respondents %>% 
  left_join(., zoning_per_grid, by = "GRID_ID") %>%
  mutate(regular_pct = regular/53*100) %>%
  mutate(pct_comm = ifelse(is.na(pct_comm) == TRUE, 0, pct_comm)) -> reg_respondents

# clean-up
rm(zoning_per_grid)

# remove geometry
st_geometry(freq_respondents) <- NULL
st_geometry(reg_respondents) <- NULL
```

First, we’ll plot the frequent data as a scatterplot with our land use
data:

``` r
ggplot(data = freq_respondents, mapping = aes(pct_comm, frequent_pct/100)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Frequent Visits and Land Use",
    x = "% of Grid Containing Commercial or University Properties",
    y = "% of Respondents with Frequent Visits"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1, by = .2)) +
  scale_x_continuous(breaks = seq(0,40, by = 5), limits = c(0,40)) -> a
```

Then we’ll calculate our correlation coefficient:

``` r
freq_respondents %>%
  select(pct_comm, frequent_pct) %>%
  corrTable(coef = "pearson", pStar = FALSE)
```

    ##                pct_comm frequent_pct
    ## pct_comm          1.000             
    ## frequent_pct  0.692 (0)        1.000

Next, we’ll repeat the process for the regular visit data, first by
plotting:

``` r
ggplot(data = reg_respondents, mapping = aes(pct_comm, regular_pct/100)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Regular Visits and Land Use",
    x = "% of Grid Containing Commercial or University Properties",
    y = "% of Respondents with Regular Visits"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1, by = .2)) +
  scale_x_continuous(breaks = seq(0,40, by = 5), limits = c(0,40)) -> b
```

We’ll also calculate a correlation coefficient for this comparison:

``` r
reg_respondents %>%
  select(pct_comm, regular_pct) %>%
  corrTable(coef = "pearson")
```

    ##              pct_comm regular_pct
    ## pct_comm        1.000            
    ## regular_pct  0.608***       1.000

Then, we’ll stitch both together using the `cowplot` package:

``` r
# create plot grid
c <- plot_grid(a, b, labels = c('A', 'B'), ncol = 1, align = "v")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 6 rows containing missing values (geom_smooth).

    ## `geom_smooth()` using formula 'y ~ x'

``` r
# save plot
ggsave(filename = here("results", "figure5.pdf"), dpi = 600, units = "in", width = 8, height = 8 )

# clean-up
rm(a, b, c)
```

Finally, we’ll put together descriptive statistics for our analysis
measures:

``` r
# create descriptive objects
freq_respondents %>%
  select(GRID_ID, frequent, frequent_pct) %>%
  left_join(., reg_respondents, by = "GRID_ID") %>%
  select(GRID_ID, frequent, frequent_pct, regular, regular_pct, pct_comm) %>%
  left_join(., crime, by = "GRID_ID") %>%
  select(-GRID_ID) %>%
  stargazer(type = "html", 
            title = "Descriptive Statistics",
            summary.stat = c("n", "mean", "sd", "min", "max"),
            covariate.labels = c("Frequent Visits, Count", "Frequent Visits, Percent of Respondents",
                                 "Regular Visits, Count", "Regular Visits, Percent of Respondents",
                                 "Zoned Commercial or University, Percent of Grid",
                                 "Estimated Total Population, Count", "Part 1 Crimes, Count",
                                 "Part 1 Crimes, Rate per 1,000 Estimated Residents"),
            out = here::here("results", "descriptives.html"))
```

    ## 
    ## <table style="text-align:center"><caption><strong>Descriptive Statistics</strong></caption>
    ## <tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Statistic</td><td>N</td><td>Mean</td><td>St. Dev.</td><td>Min</td><td>Max</td></tr>
    ## <tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Frequent Visits, Count</td><td>49</td><td>6.000</td><td>11.251</td><td>0</td><td>52</td></tr>
    ## <tr><td style="text-align:left">Frequent Visits, Percent of Respondents</td><td>49</td><td>11.321</td><td>21.228</td><td>0</td><td>98</td></tr>
    ## <tr><td style="text-align:left">Regular Visits, Count</td><td>49</td><td>6.082</td><td>8.077</td><td>0</td><td>33</td></tr>
    ## <tr><td style="text-align:left">Regular Visits, Percent of Respondents</td><td>49</td><td>11.475</td><td>15.240</td><td>0</td><td>62</td></tr>
    ## <tr><td style="text-align:left">Zoned Commercial or University, Percent of Grid</td><td>49</td><td>8.577</td><td>8.324</td><td>0.000</td><td>31.773</td></tr>
    ## <tr><td style="text-align:left">Estimated Total Population, Count</td><td>49</td><td>854.661</td><td>416.131</td><td>285.105</td><td>2,084.809</td></tr>
    ## <tr><td style="text-align:left">Part 1 Crimes, Count</td><td>49</td><td>64.286</td><td>37.587</td><td>6</td><td>181</td></tr>
    ## <tr><td style="text-align:left">Part 1 Crimes, Rate per 1,000 Estimated Residents</td><td>49</td><td>79.564</td><td>45.356</td><td>11.806</td><td>242.016</td></tr>
    ## <tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr></table>
