# sample code for Participant 104

# spatial packages
library(qualmap)  # qualitative GIS
library(sf)       # read shapefiles
library(readr)    # write csv files
library(here)     # manage file paths

# load data
grids <- st_read(here("data", "grid"), stringsAsFactors = FALSE)

# define
cluster1 <- qm_define("D-2", "E-2")
cluster2 <- qm_define("F-2", "F-3")

# validate
qm_validate(ref = grids, key = GRID_ID, value = cluster1)
qm_validate(ref = grids, key = GRID_ID, value = cluster2)

# preview
qm_preview(grids, key = GRID_ID, value = cluster1)
qm_preview(grids, key = GRID_ID, value = cluster2)

# create
cluster1 <- qm_create(ref = grids, key = GRID_ID, value = cluster1, 
                      rid = params$pid, cid = 1, category = "frequent")
cluster2 <- qm_create(ref = grids, key = GRID_ID, value = cluster2, 
                      rid = params$pid, cid = 2, category = "frequent")

# combine
clusters <- qm_combine(cluster1, cluster2)

# write
write_csv(clusters, here("data", "participants", "participant_104.csv"))

