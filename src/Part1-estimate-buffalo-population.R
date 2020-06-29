# Analysis code
# Feral buffaloes at Piratuba Biological Reserve and Maracá-Jipioca Ecological Station 
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

### Part 1. Prepare data

##----- 1 - Load libraries-----
library(here)

##----- 2 - Source files-----
source(here("src", "double-count-function.R")) # using package here to build a path to the subdirectory "bin" within "jamari"


##-----3 - Read data -----
piratuba <- read.csv(here("data", "piratuba.csv"))
maraca <- read.csv(here("data", "maraca.csv"))


##-----4 - Run double count and boot.ci functions -----

#--------------------------------------------------
# Notes:
# araguari using 15 km buffer, area.sector <- 1389.26
# nw, area.sector <- 931.878
# central, area.sector <- 1601.18
# maraca (south island only),  area.sector <- 460.89
# piratuba minus central, area.sector <- 3924.69-1601.18 = 2323.51
#--------------------------------------------------

# piratuba
# this extrapolates for the entire reserve, probably not the best because
# sites with highest densities (araguari and nw) are ove-represented 
double.count (piratuba, 1,181, 1389.26+931.878+1601.18)
ci.count(piratuba,1,181,2323.51)

# nw sector
nw <- subset(piratuba, sector== "nw")
double.count (nw, 1,181, 931.878)
ci.count(nw,1,181,931.878)

# araguari sector
araguari <- subset(piratuba, sector== "araguari")
double.count (araguari, 1,181,1389.26)
ci.count(araguari,1,181, 1389.26)

# central
central <- subset(piratuba, sector== "central")
double.count (central, 1,181,1601.18)
ci.count(central,1,181, 1601.18)

# maraca
double.count (maraca,1,181,460.89)
ci.count(maraca,1,181,460.89)


##-----5 - Alternative method: estimate separately for each sector, sum and then bootstrap (n=10000) -----

# reasons for doing it:
# 1. to estimate for the entire piratuba reserve is misleading because sites with higher density are over-represented
# 2. just running separate estimates and then summing does not generate confidence intervals for the entire reserve 

# do it as a function
combined.Estimate <- function(){
df1 <- rep(NA, 10000)    # create object to receive simulation values
araguari$iboot <- 1:nrow(araguari)
nw$iboot <- 1:nrow(nw)
central$iboot <- 1:nrow(central)
for(i in 1:10000) {    # counter
  iboot1 <- sample(1:nrow(araguari), replace=TRUE)
  bootdata1 <- araguari[araguari$iboot %in% iboot1,]
  iboot2 <- sample(1:nrow(nw), replace=TRUE)
  bootdata2 <- nw[nw$iboot %in% iboot2,]
  iboot3 <- sample(1:nrow(central), replace=TRUE)
  bootdata3 <- central[central$iboot %in% iboot3,]
  c <- double.count (bootdata1, 1,181,1389.26)[1]
  d <- double.count(bootdata2,1,181,931.878)[1]
  e <- double.count(bootdata3,1,181,1601.18)[1]
  df1[i] <- as.numeric(c)+as.numeric(d)+as.numeric(e)
  }
  ret <- list("Mean buffalo population" = mean(df1, na.rm=T), "95% CI" = round(quantile(df1, probs=c(0.025, 0.975), na.rm=T), 4),"SD" = sd(df1, na.rm=T))
  return(ret)
  #assign("combinedEstimate", df1, .GlobalEnv)
}

# run it
combined.Estimate()
