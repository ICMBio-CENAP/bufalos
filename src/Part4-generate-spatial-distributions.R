# Analysis code
# Feral buffaloes at Piratuba Biological Reserve and Maracá-Jipioca Ecological Station 
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

### Generate spatial distributions

##----- 1 - Load libraries-----
library(ggmap)
library(here)

##----- 2 - Source files-----
#source(here("src", "double-count-function.R")) # using package here to build a path to the subdirectory "bin" within "jamari"

##-----3 - Read data -----
buffalo <- read.csv(here("data", "raw-data.csv"), header=T, sep=",")

# comando para criar coluna densidade (observada para o mapa)
buffalo$observed.density <- buffalo$grupo_max/(as.numeric(buffalo$Altitude)*6/2 * as.numeric(buffalo$Compriment) *10^-6)

##-----3 - Generate spatial distributions -----

# Start with provide the lon/lat range of the data
lon <- range(buffalo$Longitude, na.rm=T)
lat <- range(buffalo$Latitude, na.rm=T)

buffalo$transec.time <- paste(buffalo$transecto, buffalo$tempo, sep=".")

# Extract the unique lat/lons and put them on a data frame
locations.buffalo <- unique(cbind(as.character(buffalo$transec.time), buffalo$Latitude, buffalo$Longitude))

locations.buffalo <- data.frame(transec.time = locations.buffalo[,1], Latitude = as.numeric(locations.buffalo[,2]), Longitude = as.numeric(locations.buffalo[,3]))

locations.buffalo <- dplyr::arrange(locations.buffalo, transec.time)

# If you have internet: Download the map from google
map <- get_map(location = c(c(lon[1],lat[1]),c(lon[2],lat[2])), zoom = 10, source = "google", maptype = "satellite")

# Plot the locations of flight subunits
ggmap(map, extent = "normal", maprange = T) + geom_point(data=locations.buffalo, aes(x = Longitude, y = Latitude), colour="black", size = 0.1)

# Plot observed densities
#ggmap(map, extent = "normal", maprange = T) + geom_point(data = locations.buffalo, aes(x = Longitude, y = Latitude, color = densityRange), size = 0.5)

# Plot as a surface
ggmap(map, extent = "device", legend = "topleft")  + stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level..), data = buffalo, geom = "polygon", size = 2, bins = 10, alpha = 0.5)


