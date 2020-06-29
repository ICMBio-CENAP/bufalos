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

# remove "water only" subunits
buffalo <- subset(buffalo, ambiente!="a")

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


##-----4 - create clean datasets using polygon sector as criteria  -----

names(buffalo)
buffalo <- buffalo[,c(21, 1,2,3,4,7,8,9,10,30)]
names(buffalo) <- c("sector", "transect", "subunit", "observer1", "observer2", "S1", "S2", "B", "grp.size", "subunit.area")

# create piratuba dataset
piratuba <- subset(buffalo, buffalo$sector == "Piratuba_ Araguari" |buffalo$sector == "Piratuba_Noroeste" |buffalo$sector == "Piratuba_central")
piratuba$sector[piratuba$sector == "Piratuba_ Araguari"] <- "araguari"
piratuba$sector[piratuba$sector == "Piratuba_Noroeste"] <- "nw"
piratuba$sector[piratuba$sector == "Piratuba_central"] <- "central"

# save csv
write.csv(piratuba, here("data", "piratuba.csv"), row.names = FALSE)

# create maraca-jipioca dataset (excluding maraca norte)
maraca <- subset(buffalo, buffalo$sector == "Maraca_sul")
write.csv(maraca, here("data", "maraca.csv"), row.names = FALSE)

##-----5 - create clean datasets using transects as criteria  -----

# Araguari
araguari <- subset(buffalo, buffalo$transect=="tn1"|buffalo$transect=="tn2"|buffalo$transect=="tn3"|buffalo$transect=="tn4"|buffalo$transect=="tn5"|buffalo$transect=="tn6"|buffalo$transect=="tn7"|buffalo$transect=="tn8"|buffalo$transect=="tn9"|buffalo$transect=="tn10"|buffalo$transect=="tn11"|buffalo$transect=="tn12"|buffalo$transect=="tn13"|buffalo$transect=="tn14"|buffalo$transect=="tn15"
				   |buffalo$transect=="tc1"|buffalo$transect=="tc2"|buffalo$transect=="tc3"|buffalo$transect=="tc4"|buffalo$transect=="tc5"|buffalo$transect=="tc6"
				   |buffalo$transect=="tz7"|buffalo$transect=="tz8"|buffalo$transect=="tz9"|buffalo$transect=="tz10"|buffalo$transect=="tz11"|buffalo$transect=="tz12"
				   |buffalo$transect=="a1"|buffalo$transect=="a2"|buffalo$transect=="a3"|buffalo$transect=="a4"|buffalo$transect=="a5"|buffalo$transect=="a6"|buffalo$transect=="a7"|buffalo$transect=="a8"
				   |buffalo$transect=="b1"|buffalo$transect=="b2"|buffalo$transect=="b3"|buffalo$transect=="b4"|buffalo$transect=="b5"|buffalo$transect=="b6")

# transects ta1-ta8 and tb1-tb6 may be considered w transects 

# Northwest
nw <- subset(buffalo, buffalo$transect=="tz1"|buffalo$transect=="tz2"|buffalo$transect=="tz3"|buffalo$transect=="tz4"|buffalo$transect=="tz5"|buffalo$transect=="tz6"
			   |buffalo$transect=="tf1"|buffalo$transect=="tf2"|buffalo$transect=="tf3"|buffalo$transect=="tf4"|buffalo$transect=="tf5"|buffalo$transect=="tf6"|buffalo$transect=="tf7"|buffalo$transect=="tf8")


#----------------------------------------------------------------------------


# helicopter dataset
#helicopter <- subset(buffalo, buffalo$transect!="ta6cessna" & buffalo$transect!="ta8cessna" & buffalo$transect!="tb2cessna" & buffalo$transect!="tb4cessna" & buffalo$transect!="tb6cessna" & buffalo$transect!="tc2cessna" & buffalo$transect!="tc4cessna" & buffalo$transect!="tc6cessna" & buffalo$transect!="tn15cessna")

# cessna transects
cessna <- buffalo[which(buffalo$transect=="ta6cessna"|buffalo$transect=="ta8cessna"|buffalo$transect=="tb2cessna"|buffalo$transect=="tb4cessna"|buffalo$transect=="tb6cessna"|buffalo$transect=="tc2cessna"|buffalo$transect=="tc4cessna"|buffalo$transect=="tc6cessna"|buffalo$transect=="tn15cessna"), ]
cessna$transect <- factor(cessna$transect)

# helicopter transects
buffalo <- subset(buffalo, buffalo$transect!="ta6cessna" & buffalo$transect!="ta8cessna" & buffalo$transect!="tb2cessna" & buffalo$transect!="tb4cessna" & buffalo$transect!="tb6cessna" & buffalo$transect!="tc2cessna" & buffalo$transect!="tc4cessna" & buffalo$transect!="tc6cessna" & buffalo$transect!="tn15cessna")
buffalo$transect <- factor(buffalo$transect)
