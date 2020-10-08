packages<-c("adehabitatHR","data.table","ggfortify","grid","move","moveVis","OpenStreetMap","pbapply","plotly","rgdal","sp","tidyverse","viridis")
sapply(packages, require, character.only=T)
OPHA_points <- read.csv(file="ophiophagus_hannah.csv")
head(OPHA_points)
#Headings for the dataset
library(ggplot2)
qaqc_plot <- ggplot() + geom_point(data=OPHA_points, 
                                   aes(utm.easting,utm.northing,
                                       color=individual.local.identifier)) +
  labs(x="Easting", y="Northing") +
  guides(color=guide_legend("Identifier"))

ggplotly(qaqc_plot)

lapply(split(OPHA_points, OPHA_points$individual.local.identifier), 
       function(x)write.csv(x, file = paste(x$individual.local.identifier[1],".csv"), row.names = FALSE))
#The anatomy of the function is as follows:
#lapply(), apply the function over a list
#split(), separates the data
#function(), compose a series of steps to be applied to the data
#write.csv(), write a csv file
#paste(), create a character string to be used for the file name

files <- list.files(path = ".", pattern = "[OPHA]+[0-9]+", full.names = TRUE)
#In the list.files command above, path = "." informs the locations, in this case the root directory, pattern = describes the way 
#the files are named, in this case OPHA followed by a number between 0-9, and full.names describes how the files will be listed.

utm_points <- cbind(OPHA_points$utm.easting, OPHA_points$utm.northing)
utm_locations <- SpatialPoints(utm_points, 
                               proj4string=CRS("+proj=utm +zone=47 +datum=WGS84"))
proj_lat.lon <- as.data.frame(spTransform(
  utm_locations, CRS("+proj=longlat +datum=WGS84")))
colnames(proj_lat.lon) <- c("x","y")
raster <- openmap(c(max(proj_lat.lon$y)+0.01, min(proj_lat.lon$x)-0.01), 
                  c(min(proj_lat.lon$y)-0.01, max(proj_lat.lon$x)+0.01), 
                  type = "bing")
raster_utm <- openproj(raster, 
                       projection = "+proj=utm +zone=47 +ellps=WGS84 +units=m +no_defs")
#In the script above, utm_point is an x,y derived from the primary dataset, utm_locations set the projection to UTM Zone 47, 
#proj_lat.lon converted the UTM points to longitude/latitude, raster uses the min/max x,y data to create a bounding box to 
#retrieve the aerial imagery, and raster_utm reprojected the imagery back to UTM Zone 47 consistent with the location in 
#Thailand. Now we can use autoplot to display the raster image file with the UTM locations as an overlay.

autoplot.OpenStreetMap(raster_utm, expand = TRUE) + theme_bw() +
  theme(legend.position="bottom") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  geom_point(data=OPHA_points, aes(utm.easting,utm.northing,
                            color=individual.local.identifier), size = 3, alpha = 0.8) +
  theme(axis.title = element_text(face="bold")) + labs(x="Easting",
                                                       y="Northing") + guides(color=guide_legend("Identifier"))
#------------------------------------------------------------
mcp_raster <- function(ophiophagus_hannah.csv){
  data <- read.csv(file = "ophiophagus_hannah.csv")
  x <- as.data.frame(data$utm.easting)
  y <- as.data.frame(data$utm.northing)
  xy <- c(x,y)
  data.proj <- SpatialPointsDataFrame(xy,data, proj4string = CRS("+proj=utm +zone=12 +ellps=WGS84 +units=m +no_defs"))
  xy <- SpatialPoints(data.proj@coords)
  mcp.out <- mcp(xy, percent=100, unout="ha")
  mcp.points <- cbind((data.frame(xy)),data$individual.local.identifier)
  colnames(mcp.points) <- c("x","y", "identifier")
  mcp.poly <- fortify(mcp.out, region = "id")
  units <- grid.text(paste(round(mcp.out@data$area,2),"ha"), x=0.85,  y=0.95,
                     gp=gpar(fontface=4, col="white", cex=0.9), draw = FALSE)
  mcp.plot <- autoplot.OpenStreetMap(raster_utm, expand = TRUE) + theme_bw() + theme(legend.position="none") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    geom_polygon(data=mcp.poly, aes(x=mcp.poly$long, y=mcp.poly$lat), alpha=0.8) +
    geom_point(data=mcp.points, aes(x=x, y=y)) + 
    labs(x="Easting (m)", y="Northing (m)", title=mcp.points$identifier) +
    theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5)) + 
    annotation_custom(units)
  mcp.plot
}

pblapply(files, mcp_raster)
#Because using the original file, creates a "population" home range polygon (labeled OPHA1)
#------------------------------------------------------------
mcp_raster <- function(OPHA2.csv){
  data <- read.csv(file = "OPHA2 .csv")
  x <- as.data.frame(data$utm.easting)
  y <- as.data.frame(data$utm.northing)
  xy <- c(x,y)
  data.proj <- SpatialPointsDataFrame(xy,data, proj4string = CRS("+proj=utm +zone=12 +ellps=WGS84 +units=m +no_defs"))
  xy <- SpatialPoints(data.proj@coords)
  mcp.out <- mcp(xy, percent=100, unout="ha")
  mcp.points <- cbind((data.frame(xy)),data$individual.local.identifier)
  colnames(mcp.points) <- c("x","y", "identifier")
  mcp.poly <- fortify(mcp.out, region = "id")
  units <- grid.text(paste(round(mcp.out@data$area,2),"ha"), x=0.85,  y=0.95,
                     gp=gpar(fontface=4, col="white", cex=0.9), draw = FALSE)
  mcp.plot <- autoplot.OpenStreetMap(raster_utm, expand = TRUE) + theme_bw() + theme(legend.position="none") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    geom_polygon(data=mcp.poly, aes(x=mcp.poly$long, y=mcp.poly$lat), alpha=0.8) +
    geom_point(data=mcp.points, aes(x=x, y=y)) + 
    labs(x="Easting (m)", y="Northing (m)", title=mcp.points$identifier) +
    theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5)) + 
    annotation_custom(units)
  mcp.plot
}

pblapply(files, mcp_raster)
#The anatomy of the function above is as follows: mcp_raster <- function(filename){... for each file listed from the project 
#directory complete the following commands
#read.csv(), read the data from a given *.csv file
#xy, create a coordinate file of the easting and northing data
#SpatialPointsDataFrame(), project the data to UTM Zone 47
#mcp(), computes home range using the Minimum Convex Polygon estimator
#fortify(), turns a map into a data frame for plotting with ggplot2
#grid.text(), creates annotations for the map; in this case to show area
#autoplot(), plotting for raster and vector data in ggplot2
#pblapply(), referencing the files list and function

kde_raster <- function(OPHA1.csv){
  data <- read.csv(file = "OPHA1 .csv")
  x <- as.data.frame(data$utm.easting)
  y <- as.data.frame(data$utm.northing)
  xy <- c(x,y)
  data.proj <- SpatialPointsDataFrame(xy,data, proj4string = CRS("+proj=utm +zone=47 +ellps=WGS84 +units=m +no_defs"))
  xy <- SpatialPoints(data.proj@coords)
  kde<-kernelUD(xy, h="href", kern="bivnorm", grid=100)
  ver <- getverticeshr(kde, 95)
  kde.points <- cbind((data.frame(data.proj@coords)),data$individual.local.identifier)
  colnames(kde.points) <- c("x","y","identifier")
  kde.poly <- fortify(ver, region = "id")
  units <- grid.text(paste(round(ver$area,2)," ha"), x=0.85,  y=0.95,
                     gp=gpar(fontface=4, col="white", cex=0.9), draw = FALSE)
  kde.plot <- autoplot.OpenStreetMap(raster_utm, expand = TRUE) + theme_bw() + theme(legend.position="none") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    geom_polygon(data=kde.poly, aes(x=kde.poly$long, y=kde.poly$lat), alpha = 0.8) +
    geom_point(data=kde.points, aes(x=x, y=y)) +
    labs(x="Easting (m)", y="Northing (m)", title=kde.points$identifier) +
    theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5)) + 
    annotation_custom(units)
  kde.plot
}

pblapply(files, kde_raster)



kde_raster <- function(filename){
  data <- read.csv(file = filename)
  x <- as.data.frame(data$utm.easting)
  y <- as.data.frame(data$utm.northing)
  xy <- c(x,y)
  data.proj <- SpatialPointsDataFrame(xy,data, proj4string = CRS("+proj=utm +zone=47 +ellps=WGS84 +units=m +no_defs"))
  xy <- SpatialPoints(data.proj@coords)
  kde<-kernelUD(xy, h="href", kern="bivnorm", grid=100)
  ver <- getverticeshr(kde, 95)
  kde.points <- cbind((data.frame(data.proj@coords)),data$individual.local.identifier)
  colnames(kde.points) <- c("x","y","identifier")
  kde.poly <- fortify(ver, region = "id")
  units <- grid.text(paste(round(ver$area,2)," ha"), x=0.85,  y=0.95,
                     gp=gpar(fontface=4, col="white", cex=0.9), draw = FALSE)
  kde.plot <- autoplot.OpenStreetMap(raster_utm, expand = TRUE) + theme_bw() + theme(legend.position="none") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    geom_polygon(data=kde.poly, aes(x=kde.poly$long, y=kde.poly$lat), alpha = 0.8) +
    geom_point(data=kde.points, aes(x=x, y=y)) +
    labs(x="Easting (m)", y="Northing (m)", title=kde.points$identifier) +
    theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5)) + 
    annotation_custom(units)
  kde.plot
}

pblapply(files, kde_raster)
#NOTE: no space in the first OPHA filename, space in second OPHA filename
#NOTE: leaving the filenames as filename makes the code loop through the list "files" which was created earlier and includes
#both OPHA1.csv and OPHA2.csv
#The anatomy of the function above is as follows: kde_raster <- function(filename){... for each file listed from the project 
#directory complete the following commands
#kernelUD(), estimation of kernel home-range
#getverticeshr(), extract home-range contour

OPHA1 <- read.csv("OPHA1 .csv")
date <- as.POSIXct(strptime(as.character(OPHA1$timestamp),"%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok"))
OPHA1$date <- date
OPHA1.reloc <- cbind.data.frame(OPHA1$utm.easting, OPHA1$utm.northing,
                                as.vector(OPHA1$individual.local.identifier),
                                as.POSIXct(date))
colnames(OPHA1.reloc) <- c("x","y","id","date")
trajectory <- as.ltraj(OPHA1.reloc, date=date, id="OPHA1")
sig1 <- liker(trajectory, sig2 = 58, rangesig1 = c(0, 5), plotit = FALSE)
opha.traj <- kernelbb(trajectory, sig1 = .7908, sig2 = 58, grid = 100)
bb_ver <- getverticeshr(opha.traj, 95)
bb_poly <- fortify(bb_ver, region = "id", 
                   proj4string = CRS("+proj=utm +zone=47+
                                     ellps=WGS84 +units=m +no_defs"))
colnames(bb_poly) <- c("x","y","order","hole","piece","id","group")
bb_image <- crop(opha.traj, bb_ver, 
                 proj4string = CRS("+proj=utm +zone=47 +
                                   ellps=WGS84 +units=m +no_defs"))
bb_units <- grid.text(paste(round(bb_ver$area,2)," ha"), x=0.85,  y=0.95,
                      gp=gpar(fontface=4, col="white", cex=0.9), draw = FALSE)
bb.plot <- autoplot.OpenStreetMap(raster_utm, expand = TRUE) + theme_bw() + theme(legend.position="none") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  geom_tile(data=bb_image, 
            aes(x=bb_image@coords[,1], y=bb_image@coords[,2],
                fill = bb_image@data$ud)) +
  geom_polygon(data=bb_poly, aes(x=x, y=y, group = group), color = "black", fill = NA) +
  scale_fill_viridis_c(option = "inferno") + annotation_custom(bb_units) +
  labs(x="Easting (m)", y="Northing (m)", title="OPHA1") +
  theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5))
bb.plot
#The anatomy of the script above is as follows:
#as.POSIXct(), manipulate objects to represent calendar dates and times
#cbind.data.frame(), used to combine columns avoiding factorization
#as.ltraj, convert data to trajectory class
#liker(), used to find the maximum likelihood estimation of the parameter sig1 in kernelbb()
#scale_fill_viridis_c(), create color scale for continuous data


brownianbridge <- function(filename){
  OPHA <- read.csv(file= filename)
  date <- as.POSIXct(strptime(as.character(OPHA$timestamp),"%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok"))
  OPHA$date <- date
  OPHA.reloc <- cbind.data.frame(OPHA$utm.easting, OPHA$utm.northing,
                                as.vector(OPHA$individual.local.identifier),
                                as.POSIXct(date))
  colnames(OPHA.reloc) <- c("x","y","id","date")
  trajectory <- as.ltraj(OPHA.reloc, date=date, id=OPHA$individual.local.identifier)
  sig1 <- liker(trajectory, sig2 = 58, rangesig1 = c(0, 5), plotit = FALSE)
  opha.traj <- kernelbb(trajectory, sig1 = .7908, sig2 = 58, grid = 100)
  bb_ver <- getverticeshr(opha.traj, 95)
  bb_poly <- fortify(bb_ver, region = "id", 
                   proj4string = CRS("+proj=utm +zone=47+
                                     ellps=WGS84 +units=m +no_defs"))
  colnames(bb_poly) <- c("x","y","order","hole","piece","id","group")
  bb_image <- crop(opha.traj, bb_ver, 
                 proj4string = CRS("+proj=utm +zone=47 +
                                   ellps=WGS84 +units=m +no_defs"))
  bb_units <- grid.text(paste(round(bb_ver$area,2)," ha"), x=0.85,  y=0.95,
                      gp=gpar(fontface=4, col="white", cex=0.9), draw = FALSE)
  bb.plot <- autoplot.OpenStreetMap(raster_utm, expand = TRUE) + theme_bw() + theme(legend.position="none") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    geom_tile(data=bb_image, 
            aes(x=bb_image@coords[,1], y=bb_image@coords[,2],
                fill = bb_image@data$ud)) +
    geom_polygon(data=bb_poly, aes(x=x, y=y, group = group), color = "black", fill = NA) +
    scale_fill_viridis_c(option = "inferno") + annotation_custom(bb_units) +
    labs(x="Easting (m)", y="Northing (m)", title=OPHA$individual.local.identifier) +
    theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5))
  bb.plot
}
pblapply(files, brownianbridge)


