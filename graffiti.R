# Store default graphing parameters so we can change them back after we customize them.
def.par <- par(no.readonly = TRUE)

## Load neccisary libraries.

if (!require("spatstat")) {
    install.packages("spatstat", dependencies = TRUE)
    library(spatstat)
}
if (!require("RColorBrewer")) {
    install.packages("RColorBrewer", dependencies = TRUE)
    library(RColorBrewer)
}
if (!require("maps")) {
    install.packages("maps", dependencies = TRUE)
    library(maps)
    library(maptools)
}
# if (!require("PBSmapping")) {
#     install.packages("PBSmapping", dependencies = TRUE)
#     library(PBSmapping)
# }
if(!require("rgdal")) {
    install.packages("rgdal", dependencies = TRUE)
    library(rdgal)
}

#set.seed(42)

# Load the data sets.
# Data gathered from city of chicago data portal at data.cityofchicago.org.
# Data gathered on 29 April 2015 at 9AM CDT.

# Main graffiti data.
# Link to dataset: https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Graffiti-Removal/hec5-y4x5
if(!exists("graffiti_orig")){
    graffiti <- read.csv("./311_Service_Requests_-_Graffiti_Removal.csv")
}

# Load the mapping shapefiles.
# Chicago shapefile.
# Link to dataset: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-City/ewy2-6yfk
if (!exists("city_border")){
    city_border <- readOGR(dsn = "./City_20Boundary", layer = "City_Boundary")
}
# Chicago neighborhood shapefiles.
# Link to dataset: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Neighborhoods/bbvz-uum9
if(!exists("neighborhood_borders")){
    neighborhood_borders <- readOGR(dsn="./Neighborhoods_2012", layer = "Neighborhoods_2012b")
}
# CTA routes shapefile.
# Link to dataset: https://data.cityofchicago.org/Transportation/CTA-L-Rail-Lines-Shapefile/53r7-y88m
if(!exists("CTA_routes")){
    CTA_routes <- readOGR(dsn = "./CTA_RailLines", layer = "CTA_RailLines")
}

# Get the colors for each of the CTA lines. (ex. Blue line, red line...)
CTA_colors <- CTA_routes@data$LEGEND
CTA_blue <- "#0099FF"; CTA_brown <- "#996633"; CTA_green <- "#00CC00"; CTA_loop <- "#CCCCCC"; CTA_orange <-  "#FF9900"
CTA_pink <- "#FF66FF"; CTA_purple <- "#6600CC"; CTA_red <- "#FF0000"; CTA_yellow <-  "#FFFF00"
levels(CTA_colors) <- c(CTA_blue, CTA_brown, CTA_green, CTA_loop, CTA_orange, CTA_pink, CTA_purple, CTA_red, CTA_yellow)
CTA_colors <- as.character(CTA_colors)

# Fix the column names and date format in the graffiti dataset.
names(graffiti) <- make.names(names(graffiti))
#graffiti <- graffiti[sample(1:nrow(graffiti), 10000), ]
graffiti$Creation.Date <- as.Date(graffiti$Creation.Date, format = "%m/%d/%Y")

# Grab and validate the lattude%longitude for records from 2014.
is.2014 <- format(graffiti$Creation.Date, "%Y") == 2014
graffiti <- graffiti[is.2014,]
# lat <- graffiti$Latitude
# long <- graffiti$Longitude
# LatLong  <- data.frame(cbind(long, lat))
LatLong  <- data.frame(cbind(graffiti$Longitude, graffiti$Latitude))
ok <- !is.na(LatLong[, 1]) & !is.na(LatLong[, 2]) & LatLong[, 2] > 40 & LatLong[, 1] < -80
LatLong <- LatLong[ok, ]

# Grab the map projection info from the shapefile, and 
# apply them the the Lat&Longs to match the shapefile coords.
# Projection: Transverse mercator with units of feet.
projstr <- city_border@proj4string@projargs
LatLong_Proj <- project(as.matrix(LatLong), projstr)

# Store the lat&long as a ppp object
xmax <- max(LatLong_Proj[, 1])
xmin <- min(LatLong_Proj[, 1])
ymax <- max(LatLong_Proj[, 2])
ymin <- min(LatLong_Proj[, 2])
LatLong_Proj <- ppp(LatLong_Proj[, 1], LatLong_Proj[, 2], window = owin(c(xmin, xmax), c(ymin, ymax)))

# Make the density map.
# graffiti_density <- density(LatLong_Proj,1000, diggle = TRUE)
graffiti_density <- density(LatLong_Proj, 1024, diggle = TRUE)

# Make the color palette for the density map.
density_colors <- c("white", "purple", "blue", "cyan")
maxDensity <- max(graffiti_density[["v"]])
minDensity <- min(graffiti_density[["v"]])
col_palette <- colorRampPalette(density_colors)(25)
col_palette <- paste(col_palette, "85", sep = "")
col_palette[1] <- "#FFFFFF00"

# Make the plot.
par(mar = c(1,1,2,1))
plot(city_border, bg = "darkgray", col = "black", border = "white")     # City Border.
plot(neighborhood_borders, col = "black", border = "white", add = TRUE) # Neighborhood borders.
plot(graffiti_density, col = col_palette, add = TRUE)                   # Density map.
plot(CTA_routes, lwd = 2, lt = 3, col = CTA_colors, add = TRUE)         # CTA routes.
# Title & Legend.
title("Chicago Graffiti Removal Requests (2014)", bg = "black", col = "white")
legend(xmin - (xmax-xmin)*0.25, ymin + (ymax - ymin) * 0.7, legend = round(seq(0, maxDensity * (5280 * 5280) - 1, maxDensity * (5280 * 5280) / 10)), 
       fill = colorRampPalette(density_colors)(10), title = "Requests per square mile")
par(def.par)