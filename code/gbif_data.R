library(ellipsenm)
library(leaflet)
library(rgdal)
library(tidyverse)
library(magrittr)
library(raster)
library(rgbif)
library(sf)
library(spocc)

aa <- occ("Aedes aegypti", geometry = c(-57.64426, -49.6921, -33.75247, -27.0810))
occs <- aa$gbif$data$Aedes_aegypti
head(occs)

# references

sp_search <- occ_search(taxonKey = aa$gbif$data$Aedes_aegypti$taxonKey[1])
cit <- gbif_citation(sp_search)
sink("data/gbif_ref.txt")
sapply(cit, print)
sink()

# saving initial data with various columns from GBIF

colnames(occs)
columns <- c(
  "name", "longitude", "latitude", "issues", "scientificName",
  "coordinateUncertaintyInMeters", "year", "month", "day",
  "countryCode"
)

occs <- occs[, columns]

# Data cleaning

colnames(occs)

# only columns of interest

occ <- occs[, c("scientificName", "longitude", "latitude", "year")]
occ <- na.omit(occ)

# defining best time interval for variables
hist(occ$year, breaks = 20)

# changing names in first column

unique(occ$scientificName)
occ <- occ[occ$scientificName == "Aedes aegypti (Linnaeus, 1762)", ]
occ$scientificName <- "Aedes aegypti"

# excluding 0, 0 coordinates

occ <- occ[occ$longitude != 0 & occ$latitude != 0, ]

# excluding duplicates
occ <- occ[!duplicated(paste(occ$longitude, occ$latitude)), ]

# write.csv(occ, "A_aegypti_clean.csv", row.names = F)

# spatial distance thinning

occt <- thin_data(occ, "longitude", "latitude",
  thin_distance = 25, save = T,
  name = "A_aegypti_25km"
)

# reading shapefile

my_spdf <- readOGR(
  dsn = paste0("data/shapefile"),
  layer = "Municipios_IBGE",
  verbose = FALSE
)

plot(my_spdf)

# Convert data.frame to SpatialPointsDataFrame

aa_sp <- aa$gbif$data$Aedes_aegypti
names(aa_sp)
coordinates(aa_sp) <- ~ longitude + latitude
crs(aa_sp) <- crs(my_spdf)
plot(aa_sp, add = T)

# Get the shapefile attributes
# Extract values to points

aa_atributtes <- over(aa_sp, my_spdf)

write.csv(aa_atributtes, "data/A_aegypti_clean.csv")

# leaflet(data = aa_atributtes) %>% addTiles() %>%
#  addCircleMarkers(~LONGITUDES, ~LATITUDESE,
#                   radius = 4,
#                   color = 'red',
#                   stroke = FALSE, fillOpacity = 0.9)
