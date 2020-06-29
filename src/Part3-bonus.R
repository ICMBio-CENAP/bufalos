
# Analysis code
# Feral buffaloes at Piratuba Biological Reserve and Maracá-Jipioca Ecological Station 
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

### Part 3. Bonus - estimate population size with alternative (conventional) method
# use transects as sampling units to estimate densities and confidence intervals (inter-transect CI and SD)
# instead of 30-second sub-units as in Part1

##----- 1 - Load libraries-----
library(here)
#library(ggplot2)

##----- 2 - Source files-----
#source(here("bin", "multiplot.R")) # using package here to build a path to the subdirectory "bin" within "jamari"


##-----3 - Read data -----
piratuba <- read.csv(here("data", "piratuba.csv"))
araguariTransects <- read.csv(here("data", "araguariTransects.csv"))
nwTransects <- read.csv(here("data", "nwTransects.csv"))
maraca <- read.csv(here("data", "maraca.csv"))


##-----4 - Estimate population density using transects as sampling units -----

# write as a function
densEstimate <- function(data, area) {
	sector.area <- area
	df1 <- cbind.data.frame(sort(unique(data$transect)), rep(NA, length(unique(data$transect))))
	colnames(df1) <- c("transect", "D")
	for(i in 1:nrow(df1))    # counter
		{
		df2 <- subset(data, data$transect == df1[i,1])
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
densEstimate(araguariTransects, 1389.26)
densEstimate(nwTransects, 931.878)
densEstimate(piratuba, 2323.51)
densEstimate(maraca, 460.89)



#--------------------------------------------





# plotting it
plot(jitter(df1$cover,2), df1$P.detec, pch=20, col=1, cex=1, main="Probabilidade de detecção", xlab="Índice de cobertura de vegetação (média por transecto", ylab="Probabilidade de detecção")

plot(jitter(df1$cover,2), df1$P.detec, ylim=c(0.5,1), pch=20, col=1, cex=1, main="Probabilidade de detecção", xlab="Índice de cobertura de vegetação (média por transecto", ylab="Probabilidade de detecção")

# acrescentando fórmula da regressão ao gráfico
cf <- round(coef(f), 2) # arredondando coeficientes para melhor output
eq <- paste0("P = ", cf[1]," + ", abs(cf[2]), "* tamanho do grupo")
## printing of the equation
mtext(eq, 3, line=-2)


#---------------IDEM para Maraca-------------------

for(i in 1:nrow(x.maraca))    # criando contador
{
	sub.x.maraca <- subset(maraca, maraca$transecto == x.maraca[i,1])
	W <- as.numeric(sub.x.maraca$Altitude)*6/2 # largura de amostragem em vôo, W = H*w/h
	area.sampled.m2 <- W*as.numeric(sub.x.maraca$Compriment) # área amostrada (m²)
	area.sampled.total <- sum(na.omit(area.sampled.m2))*10^-6 # area amostrada em km2
	B <- sum(as.numeric(sub.x.maraca$B)) # n groups seen by both observers
	S1 <- sum(as.numeric(sub.x.maraca$S1)) # n groups seen by observer 1
	S2 <- sum(as.numeric(sub.x.maraca$S2)) # n groups seen by observer 2
	P1 <- sum(sub.x.maraca$B)/(sum(sub.x.maraca$B)+sum(sub.x.maraca$S2)) # detection probability
	P2 <- sum(sub.x.maraca$B)/(sum(sub.x.maraca$B)+sum(sub.x.maraca$S1))
	M <- S1*S2/B
	y.1 <- (B+S1+1)
	y.2 <- (B+S2+1)
	y.3 <- (B+1)
	Y <- (y.1*y.2/y.3)-1
	mean.group.size <- mean(sub.x.maraca$grupo_max[sub.x.maraca$grupo_max!=0]) # mean group size ex.maracacluding zeros
	pop.estimate <- Y*mean.group.size # total population in sector
	pop.density <- pop.estimate/area.sampled.total
	x.maraca[i,2] <- pop.density
}

df1[is.na(df1)] <- 0 # transectos com zero buffalos estavam como NA, corrigir isso
piratuba.mean.density <- mean(df1$D)
piratuba.pop.TOTAL <- piratuba.mean.density*3924.69 # população mais alta porque zeros foram descartados acima
# FALTA CALCULAR O SD

x.maraca[is.na(x.maraca)] <- 0 # transectos com zero buffalos estavam como NA, corrigir isso
maraca.mean.density <- mean(na.omit(x.maraca$D))
maraca.pop.TOTAL <- maraca.mean.density*460.89


#-------------------- COMPARAR TRANSECTOS CESSNA VS HELICOPTERO-----------------------------

teste <- read.table("buffalo_20feb2018.csv", header=T, sep=",")
cessna <- subset(teste, teste$transecto=="ta6cessna"|teste$transecto=="ta8cessna"|teste$transecto=="tb2cessna"|teste$transecto=="tb4cessna"|teste$transecto=="tb6cessna"|teste$transecto=="tc28cessna"|teste$transecto=="tc4cessna"|teste$transecto=="tc6cessna"|teste$transecto=="tn15cessna")
nao.cessna <- subset(teste, teste$transecto=="ta6"|teste$transecto=="ta8"|teste$transecto=="tb2"|teste$transecto=="tb4"|teste$transecto=="tb6"|teste$transecto=="tc28"|teste$transecto=="tc4"|teste$transecto=="tc6"|teste$transecto=="tn15")

# calculando pop density para cessna
area.sampled.m2 <- 300*as.numeric(cessna$Compriment) # área amostrada (m²)
area.sampled.total <- sum(na.omit(area.sampled.m2))*10^-6 # area amostrada em km2
B <- sum(as.numeric(cessna$B)) # n groups seen by both observers
S1 <- sum(as.numeric(cessna$S1)) # n groups seen by observer 1
S2 <- sum(as.numeric(cessna$S2)) # n groups seen by observer 2
P1 <- sum(cessna$B)/(sum(cessna$B)+sum(cessna$S2)) # detection probability
P2 <- sum(cessna$B)/(sum(cessna$B)+sum(cessna$S1))
M <- S1*S2/B
y.1 <- (B+S1+1)
y.2 <- (B+S2+1)
y.3 <- (B+1)
Y <- (y.1*y.2/y.3)-1
mean.group.size <- mean(cessna$grupo_max[cessna$grupo_max!=0]) # mean group size excluding zeros
pop.estimate <- Y*mean.group.size # total population in sector
pop.density.cessna <- pop.estimate/area.sampled.total

# calculando pop density para nao.cessna (mesmos transectos)
aarea.sampled.m2 <- 300*as.numeric(nao.cessna$Compriment) # área amostrada (m²)
aarea.sampled.total <- sum(na.omit(aarea.sampled.m2))*10^-6 # area amostrada em km2
aB <- sum(as.numeric(nao.cessna$B)) # n groups seen by both observers
aS1 <- sum(as.numeric(nao.cessna$S1)) # n groups seen by observer 1
aS2 <- sum(as.numeric(nao.cessna$S2)) # n groups seen by observer 2
aP1 <- sum(nao.cessna$B)/(sum(nao.cessna$B)+sum(nao.cessna$S2)) # detection probability
aP2 <- sum(nao.cessna$B)/(sum(nao.cessna$B)+sum(nao.cessna$S1))
aM <- aS1*aS2/aB
ay.1 <- (aB+aS1+1)
ay.2 <- (aB+aS2+1)
ay.3 <- (aB+1)
aY <- (ay.1*ay.2/ay.3)-1
amean.group.size <- mean(nao.cessna$grupo_max[nao.cessna$grupo_max!=0]) # mean group size excluding zeros
apop.estimate <- aY*amean.group.size # total population in sector
pop.density.nao.cessna <- apop.estimate/aarea.sampled.total


