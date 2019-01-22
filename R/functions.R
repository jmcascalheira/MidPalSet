
#### Routine from Daniel Contreras

require(rgdal)
require(raster)
require(rasterVis)
require(lattice)


# Read DEM raster
areaDEM <- raster("XXXX.tif")

# Convert to UTM using the appropriate proj4 string (http://proj4.org/index.html)
# for the CRS (Coordinate Reference System) that we want.

areaDEMutm <- projectRaster(areaDEM,
                            crs="+proj=utm +zone=31 +datum=WGS84
                            +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# View raster info
areaDEMutm


# Read shape file with sites coordinates
sites <- readOGR(dsn="FOLDER_LOCATION", layer="FILE_NAME")


# Subset sites
#sites_sub <- sites[sites$period == "EIA" | sites$period == "GalRom",]
#sites_sub$period <- factor(sites_sub$period) # drop unused levels

# Project points to UTM
sites_sub_utm <- spTransform(sites_sub,
                             "+proj=utm +zone=31 +datum=WGS84
                             +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")



# Calculate slope
area_slope <- terrain(areaDEMutm, opt = 'slope', unit = 'degrees')

# Calculate aspect
area_aspect <- terrain(areaDEMutm, opt = 'aspect', unit = 'degrees')



# Show Map
rasterVis::levelplot(areaDEMutm,
                     margin = list(x = FALSE,
                                   y = TRUE),
                     col.regions = terrain.colors(16),
                     xlab = list(label = "",
                                 vjust = -0.25),
                     sub = list(
                       label = "masl",
                       font = 1,
                       cex = .9,
                       hjust = 1.5))


# Show Map with sites
rasterVis::levelplot(areaDEMutm,
                     margin = list(x = F,
                                   y = T),
                     col.regions = terrain.colors(16),
                     xlab = list (label = "",
                                  vjust = -.25),
                     sub = list(
                       label = "masl",
                       font = 1,
                       cex = .9,
                       hjust = 1.5),
                     key = list(       #this time we'll include a legend that identifies the points we'll plot
                       space = "top",
                       points = list(
                         pch = c(18,20),
                         col = c("red","blue")),
                       text = list(
                         c("EIA","GalRom"),
                         cex=.8))) +
  spplot(sites_sub_utm, # add a layer of points
         zcol = "period",
         cex = .6,
         pch = c(18,20),
         col.regions = c("red","blue"))



