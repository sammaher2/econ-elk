##### cody elk data ##########
# prelim looks for migration valuation plans
# kjb mar 2022

setwd("C:\\Users\\Kristin\\Box Sync\\Documents\\Collaborations\\MigrationValuation")
datDir <- "C:\\Users\\Kristin\\Box Sync\\Documents\\Data\\Elk\\AllElk_MidLab\\raw"

library(rgdal)
library(sp)
library(raster)
library(adehabitatHR)
library(tidyverse)

ll <- CRS("+init=epsg:4326") # WGS 84
utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N 



# animals.csv has animal id and herd
# gps.csv has locs, obvs

animalsRaw <- read.csv(paste0(datDir, "/animals.csv"))
gpsRaw <- read.csv(paste0(datDir, "/gps.csv")) # wowwww poor little computer
sensorsRaw <- read.csv(paste0(datDir, "/sensors.csv"))
sensorsAnimalsRaw <- read.csv(paste0(datDir, "/sensorsAnimals.csv"))

unique(animalsRaw$herd)
unique(animalsRaw$commonHerd)
names(gpsRaw)
unique(gpsRaw$gps_sensors_code)
unique(sensorsAnimalsRaw$gps_sensors_code)

animalsCody <- animalsRaw %>% filter(herd == "Cody")
codyElk <- animalsCody$animals_code
codySensors <- sensorsAnimalsRaw %>%
  dplyr::select(animals_code, gps_sensors_code) %>%
  semi_join(animalsCody)
gpsCody <- gpsRaw %>%
  semi_join(codySensors) %>%
  filter(!is.na(longitude)) %>%
  filter(longitude < -100)
head(gpsCody)
str(gpsCody)

head(gpsCody)
require(dplyr)
test <- gpsCody %>%
  mutate(year = substr(gpsCody$utc_date_time, 1,4))
unique(test$year)

gpsSpLL <- SpatialPointsDataFrame(
  data.frame("x" = gpsCody$longitude, "y" = gpsCody$latitude),
  data.frame(gps_sensors_code = gpsCody$gps_sensors_code), proj4string = ll)



kde <- kernelUD(gpsSpLL)

for (i in 1:length(kde)) {
rast <- raster(as(kde[[i]], "SpatialPixelsDataFrame"))
writeRaster(rast, paste0("testKDE-", i), format="GTiff", overwrite=TRUE)
}

kdeAll <- kernelUD(SpatialPoints(gpsSpLL), grid = 5000)
rastAll <- raster(as(kdeAll, "SpatialPixelsDataFrame"))
plot(rastAll)
crs(rastAll) <- ll
writeRaster(rastAll, "testKDE-all", format = "GTiff", overwrite = TRUE)

cute <- mcp(gpsSpLL, percent = 95)
crs(cute)
writeOGR(cute, dsn = ".", layer = "testMCP-all95", driver = "ESRI Shapefile")

write.csv(gpsCody, "locsCodyElk.csv", row.names = FALSE)

gpsExport <- SpatialPointsDataFrame(
  data.frame("x" = gpsCody$longitude, "y" = gpsCody$latitude),
  gpsCody, proj4string = ll)
writeOGR(gpsExport, dsn = ".", layer = "codyElkLocs", driver = "ESRI Shapefile")

save.image("codeRun.RData")
