# Analysis code
# Feral buffaloes at Piratuba Biological Reserve and Maracá-Jipioca Ecological Station 
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

### Prepare raw data

##----- 1 - Load libraries-----
library(here)

##----- 2 - Source files-----
source(here("src", "double-count-function.R")) # using package here to build a path to the subdirectory "bin" within "jamari"


##-----3 - Read data and do some stuff -----
buffalo <- read.csv(here("data", "raw-data.csv"), header=T, sep=",")

# calculate area per subunit
# alternative 1: fixed width, variable length
buffalo$subunit.width <- 300
buffalo$subunit.length <- buffalo$Compriment
buffalo$subunit.area.m2 <- buffalo$subunit.width*as.numeric(buffalo$subunit.length)
buffalo$subunit.area.m2[is.na(buffalo$subunit.area.m2)] <- mean(na.omit(buffalo$subunit.area.m2)) # use mean values for the NAs
buffalo$subunit.area.km2 <- buffalo$subunit.area.m2*(10^-6)

# alternative 2: variable width (due to gps altimeter), variable length
buffalo$WIDTH <- as.numeric(buffalo$Altitude)*6/2 # largura de amostragem em vôo, W = H*w/h
buffalo$subunit.area.M2 <- buffalo$WIDTH*as.numeric(buffalo$subunit.length)
buffalo$subunit.area.M2[is.na(buffalo$subunit.area.M2)] <- mean(na.omit(buffalo$subunit.area.M2)) # use mean values for the NAs
buffalo$subunit.area.KM2 <- buffalo$subunit.area.M2*(10^-6)

# compare:
summary(buffalo$subunit.area.km2); sum(buffalo$subunit.area.km2)
summary(buffalo$subunit.area.KM2); sum(buffalo$subunit.area.KM2)

## create clean datasets for each protected area
names(buffalo)
buffalo <- buffalo[,c(21, 1,2,3,4,7,8,9,10,30)]
names(buffalo) <- c("sector", "transect", "subunit", "observer1", "observer2", "S1", "S2", "B", "grp.size", "subunit.area")

# create piratuba dataset
piratuba <- subset(buffalo, buffalo$sector == "Piratuba_ Araguari" |buffalo$sector == "Piratuba_Noroeste" |buffalo$sector == "Piratuba_central")
piratuba$sector[piratuba$sector == "Piratuba_ Araguari"] <- "araguari"
piratuba$sector[piratuba$sector == "Piratuba_Noroeste"] <- "nw"
piratuba$sector[piratuba$sector == "Piratuba_central"] <- "central"

# save csv
write.csv(piratuba, "piratuba.csv", row.names = FALSE)

# create maraca-jipioca dataset (excluding maraca norte)
maraca <- subset(buffalo, buffalo$sector == "Maraca_sul")
write.csv(maraca, "maraca.csv", row.names = FALSE)

# helicopter dataset
#helicopter <- subset(buffalo, buffalo$transect!="ta6cessna" & buffalo$transect!="ta8cessna" & buffalo$transect!="tb2cessna" & buffalo$transect!="tb4cessna" & buffalo$transect!="tb6cessna" & buffalo$transect!="tc2cessna" & buffalo$transect!="tc4cessna" & buffalo$transect!="tc6cessna" & buffalo$transect!="tn15cessna")

