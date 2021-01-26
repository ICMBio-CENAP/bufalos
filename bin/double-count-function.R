
# Function to estimate buffalo populations in each sector of the protected areas

double.count <- function(data, min, max, area) # arguments: data = PA or sector, min and max = range of group size to be used, area = total area of sector
{
  sector.area <- area
  df <- subset(data, grp.size==0 | grp.size >= min)
  df <- subset(data, grp.size <= max)
  assign("df", df, .GlobalEnv)
  #W <- 300 #as.numeric(data$Altitude)*6/2 # largura de amostragem em vôo, W = H*w/h
  #area.sampled.m2 <- W*as.numeric(data$Compriment) # área amostrada (m²)
  #area.sampled.km2 <- sum(na.omit(area.sampled.m2))*10^-6 # area amostrada em km2
  #area.sampled.km2 <- 0.45*nrow(buf)
  sampled.area <- sum(df$subunit.area)
  B <- sum(as.numeric(df$B)) # n groups seen by both observers
  S1 <- sum(as.numeric(df$S1)) # n groups seen by observer 1
  S2 <- sum(as.numeric(df$S2)) # n groups seen by observer 2
  P1 <- sum(df$B)/(sum(df$B)+sum(df$S2)) # detection probability
  P2 <- sum(df$B)/(sum(df$B)+sum(df$S1))
  M <- S1*S2/B
  y.1 <- (B+S1+1)
  y.2 <- (B+S2+1)
  y.3 <- (B+1)
  Y <- ((y.1*y.2)/(y.3))-1 #Y <- (B+S1+1)(B+S2+1) / (B+1) - 1 # estimate of the size of the population (for groups)
  # var.Y <- (S1*S2(B+S1+1)(B+S2+1))/((B+1)^2(B+2)) # variance of Y
  if (max(df$grp.size) == 0) {
    mean.grp.size <- 0
  } else {
    mean.grp.size <- mean(df$grp.size[df$grp.size!=0]) # mean group size excluding zeros
  }
  pop.estimate <- Y*mean.grp.size # total population in sector
  pop.density <- pop.estimate/sampled.area
  total <-pop.density*sector.area
  assign("total", total, .GlobalEnv) # reserve this value for the ci.count function
  ret <- list("Buffalo population" = total, "Density (ind/km2)" = pop.density, "Mean group size" = mean.grp.size)
  return(ret)
}



# Function to estimate confidence intervals for population estimate

ci.count <- function(data, min, max, area)
{
  df <- data
  df$iboot <- 1:nrow(df) # index column
  boot.ci <- rep(NA, 10000)    # object to receive simulation values
  for(i in 1:10000)    # counter
  {
  iboot <- sample(1:nrow(df), replace=TRUE) # random number vector
  bootdata <- df[df$iboot %in% iboot,] # bootdata = subset data using lines equal to lines from iboot vector
  double.count(bootdata,min,max,area)
  boot.ci[i] <- total  # fill object
  assign("boot.ci", boot.ci, .GlobalEnv)
  }
  a <- round(quantile(boot.ci, probs=c(0.025, 0.975), na.rm=T), 4)    # 95% quantiles
  b <- data.frame(matrix(c(a), ncol = 2, nrow = 1, byrow = TRUE))    # object with results
  colnames(b) <- c("Q 2.5%", "Q 97.5%")    # change colnames of b
  c <- sd(boot.ci, na.rm=T) # SD 
  ret2 <- list("IC 95%" = b, "sd" = c)
  return(ret2)
}
