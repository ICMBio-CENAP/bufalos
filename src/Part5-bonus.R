
# Analysis code
# Feral buffaloes at Piratuba Biological Reserve and Maracá-Jipioca Ecological Station 
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

### Part 3. Bonus - estimate population size with alternative (conventional) method
# use transects as sampling units to estimate densities and confidence intervals (inter-transect CI and SD)
# instead of 30-second sub-units as in Part1

# obs: the code is OK, but the allocation of transects to different sectors must be revised on the map
# I will do this as soon as possible, meanwhile, DO NOT trust in the results from this section

##----- 1 - Load libraries-----
library(here)
#library(ggplot2)

##----- 2 - Source files-----
#source(here("bin", "multiplot.R")) # using package here to build a path to the subdirectory "bin" within "jamari"


##-----3 - Read data -----
piratuba <- read.csv(here("data", "piratuba.csv"))
piratuba$TR <- paste(piratuba$sector, piratuba$transect, sep=".") # to split transects that overlap two different sectors
araguari <- subset(piratuba, sector == "araguari")
nw <- subset(piratuba, sector == "nw")
central <- subset(piratuba, sector == "central")

maraca <- read.csv(here("data", "maraca.csv"))
maraca$TR <- maraca$transect

##-----4 - Estimate population density using transects as sampling units -----

# write as a function
densEstimate <- function(data, area) {
	sector.area <- area
	df1 <- cbind.data.frame(sort(unique(data$TR)), rep(NA, length(unique(data$TR))))
	colnames(df1) <- c("TR", "D")
	for(i in 1:nrow(df1))    # counter
		{
		df2 <- subset(data, data$TR == df1[i,1])
		sampled.area <- sum(df2$subunit.area)
		B <- sum(as.numeric(df2$B)) # n groups seen by both observers
		S1 <- sum(as.numeric(df2$S1)) # n groups seen by observer 1
		S2 <- sum(as.numeric(df2$S2)) # n groups seen by observer 2
		P1 <- sum(df2$B)/(sum(df2$B)+sum(df2$S2)) # detection probability
		P2 <- sum(df2$B)/(sum(df2$B)+sum(df2$S1))
		M <- S1*S2/B
		y.1 <- (B+S1+1)
		y.2 <- (B+S2+1)
		y.3 <- (B+1)
		Y <- (y.1*y.2/y.3)-1
		mean.grp.size <- mean(df2$grp.size[df2$grp.size!=0]) # mean group size excluding zeros
		pop.estimate <- Y*mean.grp.size # total population in sector
		pop.density <- pop.estimate/sampled.area
		df1[i,2] <- pop.density
	}
	df1[is.na(df1)] <- 0 # replace NaN with zeroes
	total <-mean(df1$D)*sector.area
	assign("densities", df1, .GlobalEnv)
	ret <- list("Buffalo population" = total, "Mean density" = mean(df1$D), "SD" = sd(df1$D))
	return(ret)
}

# run the function
densEstimate(araguari, 1389.26)
densities # to see the dataframe with density per transect
densEstimate(piratuba, 2323.51)

densEstimate(maraca, 460.89)

