library(rgdal)
library(raster)
library(rasterVis)
library(lattice)
library(foreach)
library(rnaturalearth)
library(tidyverse)
library(ggplot2)
library(broom)
library(foreach)
library(sf)


#Download geo tif from OSF repositorium

#osf_retrieve_file("6hrd8") %>%
#  osf_download(path = "./analysis/data/raw_data")


#Import geo tiffs
areaDEM <- raster("analysis/data/raw_data/areaDEM.tif")
areaDEM_cut <- raster("analysis/data/raw_data/areaDEM_cut.tif")
areaSLOPE_cut <- raster("analysis/data/raw_data/areaSLOPE_cut.tif")
areaASPECT_cut <- raster("analysis/data/raw_data/areaASPECT_cut.tif")
areaDIST_cut <- raster("analysis/data/raw_data/areaDIST_cut2.tif")


# Convert coordinates to longlat WGS84
areaDEM <- projectRaster(areaDEM,
                            crs="+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
areaDEM_cut <- projectRaster(areaDEM_cut,
                         crs="+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
areaSLOPE_cut <- projectRaster(areaSLOPE_cut,
                             crs="+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
areaASPECT_cut <- projectRaster(areaASPECT_cut,
                             crs="+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
areaDIST_cut <- projectRaster(areaDIST_cut,
                             crs="+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")



#Import sites
sites <- read.csv("analysis/data/raw_data/sites_short_list.csv", header=TRUE)
caves <- read.csv("analysis/data/raw_data/all_caves.csv", header=TRUE)

#Create random points
#rand <- sampleRandom(x = areaDEM_cut, size = 360, xy = TRUE, sp = TRUE)

#Import random points
#ran <- read.csv("analysis/data/raw_data/spatial_data/random_points_coordinates.csv", header=TRUE)


#Convert to shapefile
coordinates(sites)<-~Longitude+Latitude
proj4string(sites) = CRS("+init=epsg:4326")
writeOGR(sites, "analysis/data/raw_data/spatial_data/shapefiles","sites_short", "ESRI Shapefile")

coordinates(caves)<-~Longitude+Latitude
proj4string(caves) = CRS("+init=epsg:4326")
writeOGR(caves, "analysis/data/raw_data/spatial_data/shapefiles","caves", "ESRI Shapefile")

#coordinates(ran)<-~Longitude+Latitude
#proj4string(ran) = CRS("+init=epsg:4326")
#writeOGR(ran, ".","random_points", "ESRI Shapefile")




#Read shapefiles
sites_short <- readOGR(dsn="analysis/data/raw_data/spatial_data/shapefiles", layer="sites_short")
caves <- readOGR(dsn="analysis/data/raw_data/spatial_data/shapefiles", layer="caves")
rivers <- readOGR(dsn="analysis/data/raw_data/spatial_data/shapefiles", layer="RHidro_Rivers_v2")



#Filter sites by Type
sites_sub <- sites_short[sites_short$Type == "Cave" | sites_short$Type == "Open air",]
sites_sub$Type <- factor(sites_sub$Type)


#Filter main rivers by name

main_rivers <- rivers[rivers$RIO == "Minho" | rivers$RIO == "Douro" | rivers$RIO == "Tejo" |
                             rivers$RIO == "Sado" | rivers$RIO == "Guadiana",]
main_rivers$RIO <- factor(main_rivers$RIO)


#Convert coordinates to longlat WGS84
sites_sub_long <- spTransform(sites_sub, "+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

caves_long <- spTransform(caves, "+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

rivers <- spTransform(rivers,
                             "+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")



#Import rivers for plot
#rivers <- readOGR(dsn="analysis/data/raw_data/spatial_data/shapefiles/rivers", layer="AtAgua_Agsup_Rios")

# Convert coordinates
#rivers <- spTransform(rivers, "+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") # project points to UTM



# Plot DEM with rivers and site location

#jpeg("dem.jpeg", units="in", width=10, height=12, res=300)

rasterVis::levelplot(areaDEM,
                     margin = list(x = FALSE,
                                   y = TRUE),
                     col.regions = terrain.colors(16),
                     xlab = list(label = "",
                                 vjust = -0.25),
                     sub = list(
                       label = "Meters a.s.l.",
                       font = 1,
                       cex = .9,
                       hjust = 0.5),
                     colorkey=list(space="bottom"),
                     key = list(
                       space = "left",
                       points = list(
                         pch = c(18,20),
                         col = c("red","blue")),
                       text = list(
                         c("Cave","Open-air"),
                         cex=1))) +
  spplot(sites_sub_long, # add a layer of points
         zcol = "Type",
         cex = 2,
         pch = c(18,20),
         col.regions = c("red","blue"))


#dev.off()




############
# Extract variables

# Resample Distance raster
areaDIST_cut <- resample(areaDIST_cut,areaDEM_cut, resample='bilinear')

# Stack maps
terrainstack <- stack(areaDEM_cut,
                      areaSLOPE_cut,
                      areaASPECT_cut,
                      areaDIST_cut)

# Extract values from stack for each site within a 20 m radius
sites_vals <- raster::extract(terrainstack,
                      sites_sub_long,
                      buffer = 20,
                      fun = mean,
                      sp = TRUE)


# Subdivide sites for further analysis
open_air <- dplyr::filter(as.data.frame(sites_vals), Type == "Open air")
cave <- dplyr::filter(as.data.frame(sites_vals), Type == "Cave")



############
# RESAMPLING ANALYSIS


# Script adapted from Kyle Bocinsky "How To Do Archaeological Science Using R"

set.seed(9876)

# Calculate site altitude densities

open_air_altitude_densities <- open_air %$%
   areaDEM_cut %>%
   density(from = -26, to = 1993, n = 1965) %>%
   broom::tidy() %>%
   tibble::as_tibble() %>%
   dplyr::mutate(y = y * 1965) %>%
   dplyr::rename(Elevation = x,
                Frequency = y)


# Load the values into memory for fast resampling
background_altitude_values <- areaDEM_cut %>%
  values() %>%
  na.omit()

background_slope_values <- areaSLOPE_cut %>%
  values() %>%
  na.omit()

background_aspect_values <- areaASPECT_cut %>%
  values() %>%
  na.omit()

background_distance_values <- areaDIST_cut %>%
  values() %>%
  na.omit()



# Draw 1000 random samples, and calculate their densities
background_altitude_densities <- foreach::foreach(n = 1:1000, .combine = rbind) %do% {
  background_altitude_values %>%
    sample(nrow(open_air),
           replace = FALSE) %>%
    density(from = -26, to = 1993, n = 1965) %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 1965)
} %>%
  dplyr::group_by(x) %>%
  purrrlyr::by_slice(function(x){
    quantile(x$y, probs = c(0.025, 0.5, 0.975)) %>%
      t() %>%
      broom::tidy()
  }, .collate = "cols") %>%
  magrittr::set_names(c("Elevation", "Lower CI", "Frequency", "Upper CI"))


# Plot distributions
ggplot() +
  geom_line(data = background_altitude_densities,
            mapping = aes(x = Elevation,
                          y = Frequency)) +
  geom_ribbon(data = background_altitude_densities,
              mapping = aes(x = Elevation,
                            ymin = `Lower CI`,
                            ymax = `Upper CI`),
              alpha = 0.3) +
  geom_line(data = open_air_altitude_densities,
            mapping = aes(x = Elevation,
                          y = Frequency),
            color = "red")


# Draw 1000 random samples from background, and compute two-sample Wilcoxon tests (Mann-Whitney U tests)
background_altitude_MWU <- foreach(n = 1:1000, .combine = rbind) %do% {
  background_sample <- background_altitude_values %>%
    sample(nrow(open_air),
           replace = FALSE) %>%
    wilcox.test(x = open_air$areaDEM_cut,
                y = .,
                alternative = "greater",
                exact = FALSE) %>%
    broom::tidy() %>%
    tibble::as_tibble()

} %>%
  dplyr::select(statistic, p.value)


# Get the median test statistic and 95% confidence interval
background_altitude_MWU <- foreach::foreach(prob = c(0.025,0.5,0.975), .combine = rbind) %do% {
  background_altitude_MWU %>%
    dplyr::summarise_all(quantile, probs = prob)
} %>%
  t() %>%
  magrittr::set_colnames(c("Lower CI","Median","Upper CI")) %>%
  magrittr::set_rownames(c("U statistic","p-value"))



round(background_altitude_MWU, 3)




## ASPECT

areaASPECT_cut <- raster("analysis/data/raw_data/areaASPECT_cut.tif")


# Calculate random points densities
random_aspect_densities <- random_360_cut_aspect %$%
  MEAN %>%
  density(from = -1, to = 360, n = 360) %>%
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 360) %>%
  dplyr::rename(Aspect = x,
                Frequency = y)


# Load background aspect values into memory for fast resampling
background_aspect_values <- areaASPECT_cut %>%
  values() %>%
  na.omit()


# Draw 1000 random samples, and calculate their densities
background_aspect_densities <- foreach::foreach(n = 1:1000, .combine = rbind) %do% {
  background_aspect_values %>%
    sample(nrow(random_360_cut_aspect),
           replace = FALSE) %>%
    density(from = -1, to = 360, n = 360) %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 360)
} %>%
  dplyr::group_by(x) %>%
  purrrlyr::by_slice(function(x){
    quantile(x$y, probs = c(0.025, 0.5, 0.975)) %>%
      t() %>%
      broom::tidy()
  }, .collate = "cols") %>%
  magrittr::set_names(c("Aspect", "Lower CI", "Frequency", "Upper CI"))


# Plot distributions
asp <- ggplot() +
  geom_line(data = background_aspect_densities,
            mapping = aes(x = Aspect,
                          y = Frequency)) +
  geom_ribbon(data = background_aspect_densities,
              mapping = aes(x = Aspect,
                            ymin = `Lower CI`,
                            ymax = `Upper CI`),
              alpha = 0.3) +
  geom_line(data = random_aspect_densities,
            mapping = aes(x = Aspect,
                          y = Frequency),
            color = "red")


# Draw 1000 random samples from background, and compute two-sample Wilcoxon tests (Mann-Whitney U tests)
background_aspect_MWU <- foreach(n = 1:1000, .combine = rbind) %do% {
  background_sample <- background_aspect_values %>%
    sample(nrow(random_360_cut_aspect),
           replace = FALSE) %>%
    wilcox.test(x = random_360_cut_aspect$MEAN,
                y = .,
                alternative = "greater",
                exact = FALSE) %>%
    broom::tidy() %>%
    tibble::as_tibble()

} %>%
  dplyr::select(statistic, p.value)


# Get the median test statistic and 95% confidence interval
background_aspect_MWU <- foreach::foreach(prob = c(0.025,0.5,0.975), .combine = rbind) %do% {
  background_aspect_MWU %>%
    dplyr::summarise_all(quantile, probs = prob)
} %>%
  t() %>%
  magrittr::set_colnames(c("Lower CI","Median","Upper CI")) %>%
  magrittr::set_rownames(c("U statistic","p-value"))


round(background_aspect_MWU, 3)




## SLOPE


#Import geo tif
areaSLOPE_cut <- raster("analysis/data/raw_data/areaSLOPE_cut.tif")

# Calculate random points densities
random_slope_densities <- random_360_cut_slope %$%
  MEAN %>%
  density(from = 0, to = 73, n = 73) %>%
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 73) %>%
  dplyr::rename(Slope = x,
                Frequency = y)


# Load the slope values into memory for fast resampling
background_slope_values <- areaSLOPE_cut %>%
  values() %>%
  na.omit()


# Draw 1000 random samples, and calculate their densities
background_slope_densities <- foreach::foreach(n = 1:99, .combine = rbind) %do% {
  background_slope_values %>%
    sample(nrow(random_360_cut_slope),
           replace = FALSE) %>%
    density(from = 0, to = 73, n = 73) %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 73)
} %>%
  dplyr::group_by(x) %>%
  purrrlyr::by_slice(function(x){
    quantile(x$y, probs = c(0.025, 0.5, 0.975)) %>%
      t() %>%
      broom::tidy()
  }, .collate = "cols") %>%
  magrittr::set_names(c("Slope", "Lower CI", "Frequency", "Upper CI"))


# Plot distributions
slo <- ggplot() +
  geom_line(data = background_slope_densities,
            mapping = aes(x = Slope,
                          y = Frequency)) +
  geom_ribbon(data = background_slope_densities,
              mapping = aes(x = Slope,
                            ymin = `Lower CI`,
                            ymax = `Upper CI`),
              alpha = 0.3) +
  geom_line(data = random_slope_densities,
            mapping = aes(x = Slope,
                          y = Frequency),
            color = "red")


# Draw 1000 random samples from background, and compute two-sample Wilcoxon tests (Mann-Whitney U tests)
background_slope_MWU <- foreach(n = 1:1000, .combine = rbind) %do% {
  background_sample <- background_slope_values %>%
    sample(nrow(random_360_cut_slope),
           replace = FALSE) %>%
    wilcox.test(x = random_360_cut_slope$MEAN,
                y = .,
                alternative = "greater",
                exact = FALSE) %>%
    broom::tidy() %>%
    tibble::as_tibble()

} %>%
  dplyr::select(statistic, p.value)


# Get the median test statistic and 95% confidence interval
background_slope_MWU <- foreach::foreach(prob = c(0.025,0.5,0.975), .combine = rbind) %do% {
  background_slope_MWU %>%
    dplyr::summarise_all(quantile, probs = prob)
} %>%
  t() %>%
  magrittr::set_colnames(c("Lower CI","Median","Upper CI")) %>%
  magrittr::set_rownames(c("U statistic","p-value"))


round(background_slope_MWU, 3)


grid.arrange(alt, asp, slo)
