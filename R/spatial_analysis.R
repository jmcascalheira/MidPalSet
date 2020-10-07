library(rgdal)
library(raster)
library(rasterVis)
library(lattice)

#Import geo tif
areaDEM <- raster("analysis/data/raw_data/areaDEM.tif")
areaLITO <- raster("analysis/data/raw_data/lito_raster2.tif")

areaDEMutm <- projectRaster(areaDEM,
                            crs="+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
areaLITOutm <- projectRaster(areaLITO,
                            crs="+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")


#Import sites
sites <- read.csv("analysis/data/raw_data/sites_short_list_2.csv", header=TRUE)


#Import random points
ran <- read.csv("analysis/data/raw_data/spatial_data/random_points_coordinates.csv", header=TRUE)

#Import UP caves points
caves <- read.csv("analysis/data/raw_data/spatial_data/UP_caves.csv", header=TRUE)

#Convert to shapefile
coordinates(sites)<-~Longitude+Latitude
proj4string(sites) = CRS("+init=epsg:4326")
writeOGR(sites, ".","sites_short", "ESRI Shapefile")


coordinates(ran)<-~Longitude+Latitude
proj4string(ran) = CRS("+init=epsg:4326")
writeOGR(ran, ".","random_points", "ESRI Shapefile")


coordinates(caves)<-~Longitude+Latitude
proj4string(caves) = CRS("+init=epsg:4326")
writeOGR(caves, ".","UPcaves", "ESRI Shapefile")

#Read shapefiles
sites_short <- readOGR(dsn=".", layer="sites_short")
random_points <- readOGR(dsn=".", layer="random_points")
UPcaves <- readOGR(dsn=".", layer="UPcaves")


#Filter sites by Type
sites_sub <- sites_short[sites_short$Type == "Cave" | sites_short$Type == "Open air",]
sites_sub$Type <- factor(sites_sub$Type)

#Convert coordinates to UTM
sites_sub_utm <- spTransform(sites_sub,
                             "+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") # project points to UTM

random_points_utm <- spTransform(random_points,
                             "+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") # project points to UTM

UPcaves_points_utm <- spTransform(UPcaves,
                                 "+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") # project points to UTM

#Calculate slope
area_slope <- terrain(areaDEMutm, opt = 'slope', unit = 'degrees')

#Calculate aspect
area_aspect <- terrain(areaDEMutm, opt = 'aspect', unit = 'degrees')


#Import rivers
rivers <- readOGR(dsn="analysis/data/raw_data/rivers", layer="AtAgua_Agsup_Rios")
rivers_utm <- spTransform(rivers,
            "+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") # project points to UTM



# View just DEM

jpeg("dem.jpeg", units="in", width=10, height=12, res=300)

rasterVis::levelplot(areaDEMutm,
                     margin = list(x = FALSE,
                                   y = TRUE),
                     col.regions = terrain.colors(16),
                     xlab = list(label = "",
                                 vjust = -0.25),
                     sub = list(
                       label = "Meters a.s.l.",
                       font = 1,
                       cex = .9,
                       hjust = 1.5))

dev.off()


# View Lithography

jpeg("lithography.jpeg", units="in", width=6, height=8, res=300)

rasterVis::levelplot(areaLITOutm,
                     col.regions = terrain.colors(20),
                     margin = list(x = FALSE,
                                   y = FALSE),
                     xlab = list(label = "",
                                 vjust = -0.25))

dev.off()


# View slope
rasterVis::levelplot(area_slope,
                     margin = list(x = FALSE,
                                   y = FALSE),
                     xlab = list(label = "",
                                 vjust = -0.25))

rasterVis::levelplot(area_aspect,
                     margin = list(x = FALSE,
                                   y = FALSE),
                     xlab = list(label = "",
                                 vjust = -0.25))



# View DEM with points

jpeg("site_map.jpeg", units="in", width=6, height=8, res=300)

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
                     key = list(
                       space = "left",
                       points = list(
                         pch = c(18,20),
                         col = c("red","blue")),
                       text = list(
                         c("Cave","Open-air"),
                         cex=1))
) +
  spplot(sites_sub_utm, # add a layer of points
         zcol = "Type",
         cex = 2,
         pch = c(18,20),
         col.regions = c("red","blue")
  )


dev.off()


png("analysis/figures/UPcaves_map.png", units="in", width=6, height=8, res=300)

rasterVis::levelplot(areaDEMutm,
                     margin = list(x = F,
                                   y = F),
                     col.regions = terrain.colors(16),
                     colorkey = FALSE) +
  spplot(UPcaves_points_utm, cex = 1, col.regions = "black")


dev.off()



# Stack maps
terrainstack <- stack(areaDEMutm,
                      area_slope,
                      area_aspect)


# Extract values from stack for each site within a 20 m radius
sites_vals <- extract(terrainstack,
                      sites_sub_utm,
                      buffer = 200,
                      fun = mean,
                      sp = TRUE)

# Count sites by category
summary(sites_vals$Type)


# Boxplot
elevplot <- bwplot(areaDEM ~ Type,
                   data = data.frame(sites_vals),
                   notch = TRUE,
                   pch = "|",
                   fill = "grey",
                   box.ratio = 0.25,
                   par.settings = list(
                     box.rectangle = list(
                       col = c("red","blue"))),
                   ylab = "masl",
                   main="Elevation",
                   scales = list(x = list(labels = c("Caves\n(n = 17)",
                                                     "Open air\n(n = 38)")),
                                 rot=60))
elevplot
