library(tidyverse)

# Load FER database
new_db <- read.csv("Data/master_db_V1.0.0.csv")

# Create row number for merging with points later
new_db$Row <- seq.int(nrow(new_db))

######### Fixing Project Name Characters ######### 

# Some special characters are causing coding issues (e.g., Joe's Bayou ==Joe‚Äôs Bayou)

# Function to fix 
fix_mojibake <- function(x) {
  needs_fix <- grepl("‚Ä", x, fixed = TRUE)
  
  safe_convert <- function(s) {
    out <- tryCatch(iconv(s, from = "UTF-8", to = "macroman"), error = function(e) NA_character_)
    if (is.na(out)) return(s)   # couldn't safely convert — leave it untouched rather than error
    Encoding(out) <- "UTF-8"
    out
  }
  
  x[needs_fix] <- vapply(x[needs_fix], safe_convert, character(1), USE.NAMES = FALSE)
  x <- gsub("\u200B", "", x)    # strip invisible zero-width spaces everywhere
  x
}

# Apply to field
new_db$Project_Name <- fix_mojibake(new_db$Project_Name)



######### Setting Up Spatial Data ######### 

library(sf)

# Load map of estuarine regions
estuaries <- st_read("Data/Raw/FL_EDA_NNC_boundary_estuaries.shp")

# Load map of counties (NNC clipped and full)
counties <- st_read("Data/Raw/Counties_NNC_area.shp")
counties_full <- st_read("Data/Raw/Counties_Extended.shp")


#### ------- 1. Create points from FER database  ####

# Remove any without coordinates
new_db_spatial <- new_db %>%
  filter(!is.na(Longitude), !is.na(Latitude))

# Create points
points <- st_as_sf(new_db_spatial, coords = c("Longitude","Latitude"), crs = 4326)

# Align CRS of other shapefiles
estuaries <- st_transform(estuaries, 4326)
counties <- st_transform(counties, 4326)
counties_full <- st_transform(counties_full, 4326)

# Confirm correct mapping
plot(st_geometry(estuaries), col = "grey90")
plot(st_geometry(points), bg = "#2C7FB8", col = "black", pch = 21, add = TRUE)

plot(st_geometry(counties), col = "grey90")
plot(st_geometry(points), bg = "#2C7FB8", col = "black", pch = 21, add = TRUE)


#### ------- 2. Add correct county info to database based on point locations  ####

# Some counties have invalid geometries, so fix
counties <- counties %>%
  st_make_valid()
counties_full <- counties_full %>%
  st_make_valid()

# Extract county name by point location
points <- points %>%
  st_join(counties %>% select(Name))

# Join the county name field back to the respective project in the database
new_db <- new_db %>%
  left_join(points %>% select(Row, Name),by = "Row")



######### Generate Final Datasets for Dashboard ######### 

# Database

# Exclude fields that aren't needed
#final_db <- new_db %>%
#  select(-c("X", "Name", "geometry"))

#write.csv(final_db, file = "Data/master_db_V1.0.0.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Points

points <- points %>%
  select(Row)
final_points <- points %>%
  left_join(new_db, by = "Row") %>%
  select(-Row)

save(final_points, file = "Data/dashboard_points_V1.0.0.RData")



# Estuarine Regions

# Harmonize EDA names
estuaries_final <- estuaries %>%
  mutate(EDA = str_remove(Name, " EDA$")) %>%
  select(EDA)

save(estuaries_final, file = "Data/estuaries.RData")



# Counties (NNC clip)

# Convert to title case
counties_nnc <- counties %>%
  mutate(County = str_to_title(Name)) %>%
  mutate(County = ifelse(County == "Desoto", "DeSoto", County)) %>%
  select(County)

save(counties_nnc, file = "Data/counties_NNC.RData")

# Counties (full)

counties_final <- counties_full %>%
  mutate(County = str_to_title(Name)) %>%
  select(County)

save(counties_final, file = "Data/counties.RData")


