#--------------------------------------------

# plotting it
plot(jitter(df1$cover,2), df1$P.detec, pch=20, col=1, cex=1, main="Probabilidade de detecção", xlab="Índice de cobertura de vegetação (média por transecto", ylab="Probabilidade de detecção")

plot(jitter(df1$cover,2), df1$P.detec, ylim=c(0.5,1), pch=20, col=1, cex=1, main="Probabilidade de detecção", xlab="Índice de cobertura de vegetação (média por transecto", ylab="Probabilidade de detecção")

# acrescentando fórmula da regressão ao gráfico
cf <- round(coef(f), 2) # arredondando coeficientes para melhor output
eq <- paste0("P = ", cf[1]," + ", abs(cf[2]), "* tamanho do grupo")
## printing of the equation
mtext(eq, 3, line=-2)


##-----5 - Compare helicopter and cessna transects -----

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


