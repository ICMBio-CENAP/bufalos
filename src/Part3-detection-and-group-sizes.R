# Analysis code
# Feral buffaloes at Piratuba Biological Reserve and Maracá-Jipioca Ecological Station 
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

### Part 3. Detection rates

##----- 1 - Load libraries-----
library(here)

##----- 2 - Source files-----
#source(here("bin", "double-count-function.R")) # using package here to build a path to the subdirectory "bin" within "jamari"


##-----3 - Read data -----
piratuba <- read.csv(here("data", "piratuba.csv"))
maraca <- read.csv(here("data", "maraca.csv"))

buffalo <- rbind(piratuba, maraca) # join the two reserves in a single dataframe

##-----4 - Plot group sizes -----

# Function to calculate detection probability for different group sizes

detection <- function(data, min, max) { # function of data and range of group sizes to be estimated
  group.data <- subset(data, grp.size == 0 | grp.size >= min)
  group.data <- subset(group.data, grp.size <= max)
  B <- sum(as.numeric(group.data$B)) # n groups seen by both observers
  S1 <- sum(as.numeric(group.data$S1)) # n groups seen by observer 1
  S2 <- sum(as.numeric(group.data$S2)) # n groups seen by observer 2
  P1 <- sum(group.data$B)/(sum(group.data$B)+sum(group.data$S2)) # detection probability
  P2 <- sum(group.data$B)/(sum(group.data$B)+sum(group.data$S1))
  mean.P <- (P1+P2)/2
  assign("B", B, .GlobalEnv) # saving parameters in global environment to use in next function
  assign("S1", S1, .GlobalEnv) 
  assign("S2", S2, .GlobalEnv) 
  assign("P1", P1, .GlobalEnv) 
  assign("P2", P2, .GlobalEnv) 
  assign("P", mean.P, .GlobalEnv)
  return(list("Probability of detection for selected group size", "Mean detec. prob."  =  mean.P, "Detec. prob. observer 1"  = P1, "Detec. prob. observer 2"  = P2))
}


detection(buffalo,1,1) # detection for solitary individuals
detection(buffalo,2,2) # detection for groups of two individuals
detection(buffalo,3,3) # etc
detection(buffalo,4,4)
detection(buffalo,5,5)
detection(buffalo,6,6)
detection(buffalo,7,7)
detection(buffalo,8,8)
detection(buffalo,9,9)
detection(buffalo,10,10)
detection(buffalo,1,181)

# using the detections above to create a vector an then creating a vector of group sizes
mean.det.probs <- c(0.5552941, 0.6410256, 0.5833333, 0.4875, 0.675, 0.7291667, 0.875, 0.9166667, NA, 0.8333333, 0.8601399)
group.size <- c(1:11)
det.probs.obs1 <- c(.47,.66,.58,.37,.6,.62,.75,.83,NA,.67,.78)
det.probs.obs2 <- c(.64,.61,.5,.6,.75,.83,1,1,NA,1,.93)
det.probs.obs1and2 <- c(.47,.66,.58,.37,.6,.62,.75,.83,NA,.67,.78,.64,.61,.5,.6,.75,.83,1,1,NA,1,.93)
group.size.obs1and2 <- (c(1:11,1:11))

# plotting it (write as a function)
detPLOT <- function() {
  f <- lm(det.probs.obs1and2~group.size.obs1and2)
  X <- (c(1:11,1:11))
  Y <- predict(f, newdata=data.frame(x=X))
  plot(group.size.obs1and2, det.probs.obs1and2, xaxt="n", ylim=c(0,1), pch=16, col=1, cex=1, cex.axis=1.2, xlab="Group size", ylab="Detection probability", cex.lab=1.2, las=1)
  #axis(1, at=1:11, labels=c(seq(1:10), ">10"))
  axis(1, at=1:11, labels=c("1","","3","","5","","7","","9","",">10"), cex.axis=1.2)
  lines(x=X, y=Y)
  }

detPLOT()

# save as jpeg
jpeg(here("results", "detection.jpg"), width = 600, height = 500) # Open jpeg file
detPLOT()
dev.off()

