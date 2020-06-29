# Analysis code
# Feral buffaloes at Piratuba Biological Reserve and Maracá-Jipioca Ecological Station 
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

### Part 1. Prepare data

##----- 1 - Load libraries-----
library(ggplot2)
library(here)

##----- 2 - Source files-----
source(here("src", "double-count-function.R")) # using package here to build a path to the subdirectory "bin" within "jamari"


##-----3 - Read data -----
piratuba <- read.csv(here("data", "piratuba.csv"))
maraca <- read.csv(here("data", "maraca.csv"))

# comando para criar coluna densidade (observada para o mapa)
# buffalo$dens.observ <- buffalo$grupo_max/(as.numeric(buffalo$Altitude)*6/2 * as.numeric(buffalo$Compriment) *10^-6)

##-----4 - Run double count and boot.ci functions -----

#--------------------------------------------------
# Notes:
# araguari using 15 km buffer, area.sector <- 1389.26
# nw, area.sector <- 931.878
# central, area.sector <- 1601.18
# maraca (south island only),  area.sector <- 460.89
# piratuba minus central, area.sector <- 3924.69-1601.18 = 2323.51
#--------------------------------------------------

double.count (piratuba, 1,181, 1389.26+931.878+1601.18) # extrapola igualmente para toda a reserva
ci.count(piratuba,1,181,2323.51)

double.count (w.nw.setor, 1,181, 931.878)
ci.count(w.nw.setor,1,181,931.878)

double.count (araguari.setor, 1,181,1389.26)
ci.count(araguari.setor,1,181, 1389.26)

double.count (central.setor, 1,181,1601.18)
ci.count(central.setor,1,181, 1601.18)

double.count(cessna, 1,181, 1389.26) # não funciona, aparentemente não contamos grupos nas fotos de cessna, checar

double.count (maraca,1,181,460.89)
ci.count(maraca,1,181,460.89)

#------------estimando separadamente Araguari, NW e central e depois somando, 10 mil boostrap----------------------

combinado <- rep(NA, 10000)    # criando objeto para receber valores de simulação
araguari.setor$iboot <- 1:nrow(araguari.setor)
w.nw.setor$iboot <- 1:nrow(w.nw.setor)
central.setor$iboot <- 1:nrow(central.setor)
for(i in 1:10000)    # criando contador
{
  iboot1 <- sample(1:nrow(araguari.setor), replace=TRUE)
  bootdata1 <- araguari.setor[araguari.setor$iboot %in% iboot1,]
  iboot2 <- sample(1:nrow(w.nw.setor), replace=TRUE)
  bootdata2 <- w.nw.setor[w.nw.setor$iboot %in% iboot2,]
  iboot3 <- sample(1:nrow(central.setor), replace=TRUE)
  bootdata3 <- w.nw.setor[central.setor$iboot %in% iboot3,]
  c <- double.count (bootdata1, 1,181,1389.26)[1]
  d <- double.count(bootdata2,1,181,931.878)[1]
  e <- double.count(bootdata3,1,181,1601.18)[1]
  combinado[i] <- as.numeric(c)+as.numeric(d)+as.numeric(e)
  assign("combinado", combinado, .GlobalEnv)
  }
mean(combinado)
round(quantile(combinado, probs=c(0.025, 0.975), na.rm=T), 4)    # criando objeto com quantis referentes ao 95% CI
sd(combinado)

#------------Checando alguns resultados de Tomas et al. 2011---------------------

# densidade media REBIO = 19,6 ind/km2 (regioes noroeste, oeste e sul)
# população total REBIO = 33354 buffalos
# 33354/19.6 = 1701.735 km2 = 170173 ha
# REBIO tem 392469 ha, logo população calculada para 43% da UC (meu calculo é para 59% da UC)

# densidade noroeste/oeste = 9 ind/km2
# população noroeste/oeste = 11759 ind
# 11759/9 = 1306.5 km2 = 130650 ha (meu calculo é para 93187 ha)

# densidade araguari = 51 ind/km2
# população araguari = 20703 ind
# 20703/51 = 405.9 km2 = 40000 ha (meu calculo é para 138926 ha)

 
#--------------------GRÁFICO DE TENDENCIAS TEMPORAIS-------------------------------------------------

popPIRATUBA <- c(33354, 31887, 17769)
popMARACA <- c(641,528,686.51)
sdevPIRATUBA <- c(4680, 3442, 2281.961)
sdevMARACA <- c(235,187,325.3)
ano <- c(2007, 2013, 2017)

# REBIO Piratuba
plot(ano, popPIRATUBA, ylim=range(c(0, 40000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, xlab="Ano", ylab="População de búfalos (média +/- SD)",
     main="População de búfalos na REBIO Piratuba, 2007-2017")
axis(1, seq(2007,2017,1),las=2, cex.axis=0.8, font=2)
arrows(ano, popPIRATUBA-sdevPIRATUBA, ano, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
lines(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=16) # linha entre pontos

# ESEC Maraca
plot(ano, popMARACA, ylim=range(c(0, 1000)), xlim=range(c(2007,2017)) , xaxt="n", pch=19, xlab="Ano", ylab="População de búfalos (média +/- SD)",
     main="População de búfalos na ESEC Maracá-Jipioca, 2008-2017")
axis(1, seq(2007,2017,1),las=2, cex.axis=0.8, font=2)
arrows(ano, popMARACA-sdevMARACA, ano, popMARACA+sdevMARACA, length=0.05, angle=90, code=3) # barra de SD
lines(ano[order(ano)], popMARACA[order(ano)], xlim=range(ano), ylim=range(popMARACA), pch=16) # linha entre pontos

#------------------------------------------------------------------

# Plot trends from IBGE data: https://cidades.ibge.gov.br/brasil/ap/tartarugalzinho/pesquisa/18/16459?indicador=16535&ano=2017&localidade1=160021&localidade2=160010
# efetivo rebanho bubalino IBGE
year <- c(2007:2017)
tartarugalzinho <- c(25981, 26798, 24705, 26987, 28174, 30177, 31625, 33396, 42328, 45711, 47195)
pracuuba <- c(15349, 14931, 15707, 16348, 18197, 19710, 20813, 20020, 12129, 11108, 8790)
amapa <- c(32579, 30714, 29117, 30453, 32456, 34191, 35729, 37465, 34177, 32155, 33793)
cutias <- c(48753, 49815, 51195, 53641, 62548, 65970, 69796, 72936, 75282, 76980, 79113)
all <- tartarugalzinho+pracuuba+amapa+cutias
amapa.pracuuba.tartarugalzinho <- amapa+pracuuba+tartarugalzinho
amapa.pracuuba <- amapa+pracuuba
cutias.tartarugalzinho <- cutias+tartarugalzinho

# pracuuba
plot(year, pracuuba, ylim=range(c(0, 25000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=16) # linha entre pontos
#mtext("(a)", side=3, adj=0.05, line=-1.3)
mtext("Efetivo de rebanho bubalino Pracuúba (IBGE)", side=3, line=1, cex=0.8)

# amapa
plot(year, amapa, ylim=range(c(0, 40000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], amapa[order(year)], xlim=range(year), ylim=range(amapa), pch=16) # linha entre pontos
#mtext("(a)", side=3, adj=0.05, line=-1.3)
mtext("Efetivo de rebanho bubalino Amapá (IBGE)", side=3, line=1, cex=0.8)

# tartarugalzinho
plot(year, tartarugalzinho, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=16) # linha entre pontos
#mtext("(a)", side=3, adj=0.05, line=-1.3)
mtext("Efetivo de rebanho bubalino Tartarugalzinho (IBGE)", side=3, line=1, cex=0.8)

# amapa AND pracuuba and tartarugalzinho
plot(year, amapa, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=2, cex=0.8,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], amapa[order(year)], xlim=range(year), ylim=range(amapa), pch=16, lty=3) # linha entre pontos
points(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=20, cex=0.8)
lines(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=16, lty=1) # linha entre pontos
points(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=0, cex=0.8)
lines(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=16, lty=5) # linha entre pontos
legend(2007, 13000, legend=c("Amapa", "Pracuuba", "Tartarugalzinho"), pch=c(2,20,0), lty=c(3,1,5),  cex=0.7, box.lty=0)

#-----------
# Fig 3 multiplot

par(mfrow = c(3, 1), tcl=-0.5)
par(mai = c(0.2,0.2,0.2,0.2), mar = c(0.5,0.5,0.5,0.5), oma = c(2, 6, 2, 2)) # edit oma to get space for species names
#par(mai = c(0.1,1,0.1,0.1))

# ESEC Maraca
plot(ano, popMARACA, ylim=range(c(0, 1200)), xlim=range(c(2007,2017)) , xaxt="n", yaxt="n", pch=19, xlab="Ano", ylab="População de búfalos (média +/- SD)",
     main="", cex.axis=1, las=1)
axis(2, seq(0,1200,400), cex.axis=1.5, font=1, las=1)
arrows(ano, popMARACA-sdevMARACA, ano, popMARACA+sdevMARACA, length=0.05, angle=90, code=3) # barra de SD
lines(ano[order(ano)], popMARACA[order(ano)], xlim=range(ano), ylim=range(popMARACA), pch=16) # linha entre pontos
mtext("(a)", side=3, adj=0.05, line=-2.5, cex=1.5)

# REBIO Piratuba
plot(ano, popPIRATUBA, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", yaxt ="n", pch=19, xlab="Ano", ylab="População de búfalos (média +/- SD)",
     main="", cex.axis=1, las=1)
axis(2, seq(0,50000,20000), cex.axis=1.5, font=1, las=1)
arrows(ano, popPIRATUBA-sdevPIRATUBA, ano, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
lines(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=16) # linha entre pontos
mtext("(b)", side=3, adj=0.05, line=-2.5, cex=1.5)

# amapa AND pracuuba and tartarugalzinho
plot(year, amapa, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", yaxt ="n", pch=2, cex=0.8,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=1.5, font=1, las=1)
axis(2, seq(0,50000,15000), cex.axis=1.5, font=1, las=1)
lines(year[order(year)], amapa[order(year)], xlim=range(year), ylim=range(amapa), pch=16, lty=3) # linha entre pontos
points(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=20, cex=0.8)
lines(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=16, lty=1) # linha entre pontos
points(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=0, cex=0.8)
lines(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=16, lty=5) # linha entre pontos
#legend(2007, 15000, legend=c("Amapa", "Pracuuba", "Tartarugalzinho"), pch=c(2,20,0), lty=c(3,1,5),  cex=0.8, box.lty=0)
mtext("(c)", side=3, adj=0.05, line=-2.5, cex=1.5)
text(c(2016.7,2016.7,2016.7),c(49695,36293,11290),labels=c("Tartarugalzinho", "Amapá","Pracuuba"), cex=1.5)
#text(c(2016.8,2016.8,2016.8),c(43195,29793,4790),labels=c("Tartarugalzinho", "Amapá","Pracuuba"), cex=1.5)
#-----------

#-----------
# Fig 3 multiplot version 2

par(mfrow = c(3, 1), tcl=-0.5)
par(mai = c(0.2,0.2,0.2,0.2), mar = c(0.5,0.5,0.5,0.5), oma = c(2, 6, 2, 2)) # edit oma to get space for species names
#par(mai = c(0.1,1,0.1,0.1))

# ESEC Maraca
plot(ano, popMARACA, ylim=range(c(0, 1200)), xlim=range(c(2007,2017)) , xaxt="n", yaxt="n", pch=19, xlab="", ylab="",
     main="", cex.axis=2, las=1)
axis(2, seq(0,1200,400), cex.axis=2, font=1, las=1)
arrows(ano, popMARACA-sdevMARACA, ano, popMARACA+sdevMARACA, length=0.05, angle=90, code=3) # barra de SD
lines(ano[order(ano)], popMARACA[order(ano)], xlim=range(ano), ylim=range(popMARACA), pch=16) # linha entre pontos
mtext("(a)", side=3, adj=0.05, line=-2.5, cex=1.5)

# REBIO Piratuba
plot(ano, popPIRATUBA, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", yaxt ="n", pch=19, xlab="", ylab="",
     main="", cex.axis=2, las=1)
axis(2, seq(0,50000,20000), cex.axis=2, font=1, las=1)
arrows(ano, popPIRATUBA-sdevPIRATUBA, ano, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
lines(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=16) # linha entre pontos
mtext("(b)", side=3, adj=0.05, line=-2.5, cex=1.5)

# amapa AND pracuuba and tartarugalzinho
plot(year, amapa, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", yaxt ="n", pch=2, cex=0.8,  ylab="", xlab="", las=1)
axis(1, seq(2007,2017,2), cex.axis=2, font=1, las=1)
axis(2, seq(0,50000,15000), cex.axis=2, font=1, las=1)
lines(year[order(year)], amapa[order(year)], xlim=range(year), ylim=range(amapa), pch=16, lty=3) # linha entre pontos
points(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=20, cex=0.8)
lines(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=16, lty=1) # linha entre pontos
points(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=0, cex=0.8)
lines(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=16, lty=5) # linha entre pontos
#legend(2007, 15000, legend=c("Amapa", "Pracuuba", "Tartarugalzinho"), pch=c(2,20,0), lty=c(3,1,5),  cex=0.8, box.lty=0)
mtext("(c)", side=3, adj=0.05, line=-2.5, cex=1.5)
text(c(2016.7,2016.7,2016.7),c(49695,36293,11290),labels=c("Tartarugalzinho", "Amapá","Pracuuba"), cex=1.5)
#text(c(2016.8,2016.8,2016.8),c(43195,29793,4790),labels=c("Tartarugalzinho", "Amapá","Pracuuba"), cex=1.5)
#-----------



#----------

# Piratuba AND amapa AND pracuuba
plot(year, amapa, ylim=range(c(0, 40000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=2, cex=0.8,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], amapa[order(year)], xlim=range(year), ylim=range(amapa), pch=16, lty=3) # linha entre pontos
points(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=0, cex=0.8)
lines(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=16, lty=5) # linha entre pontos
points(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=20, cex=1)
lines(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=16, lty=1) # linha entre pontos
#arrows(ano, popPIRATUBA-sdevPIRATUBA, ano, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
# Add a legend
legend(2007, 12000, legend=c("LPBR", "Amapa", "Pracuuba"), pch=c(20,2,0), lty=c(1,3,5),  cex=0.9, box.lty=0)
#mtext("LPBR", side=3, adj=0.05, line=-10, at=2016, cex=0.7)
#mtext("Amapá+Pracuúba", side=3, adj=0.05, line=-4, at=2015.3, cex=0.7)
#mtext("Buffalo heads at LPBR (dashed) and in Amapa+Pracuuba (continuous)", side=3, line=1, cex=0.8)


# amapa and pracuuba
plot(year, amapa.pracuuba, ylim=range(c(0, 60000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], amapa.pracuuba[order(year)], xlim=range(year), ylim=range(amapa.pracuuba), pch=16) # linha entre pontos
#mtext("(a)", side=3, adj=0.05, line=-1.3)
mtext("Efetivo de rebanho bubalino Amapá e Pracuuba (IBGE)", side=3, line=1, cex=0.8)


# cutias
plot(year, cutias, ylim=range(c(0, 80000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], cutias[order(year)], xlim=range(year), ylim=range(cutias), pch=16) # linha entre pontos
#mtext("(a)", side=3, adj=0.05, line=-1.3)
mtext("Efetivo de rebanho bubalino Cutias (IBGE)", side=3, line=1, cex=0.8)

  # tartarugalzinho
  plot(year, tartarugalzinho, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
  axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
  lines(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=16) # linha entre pontos
  #mtext("(a)", side=3, adj=0.05, line=-1.3)
  mtext("Efetivo de rebanho bubalino Tartarugalzinho (IBGE)", side=3, line=1, cex=0.8)

# all
plot(year, all, ylim=range(c(0, 200000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], all[order(year)], xlim=range(year), ylim=range(all), pch=16) # linha entre pontos
#mtext("(a)", side=3, adj=0.05, line=-1.3)
mtext("Efetivo de rebanho bubalino around LPBR (IBGE)", side=3, line=1, cex=0.8)

# amapa.pracuuba.tartarugalzinho
plot(year, amapa.pracuuba.tartarugalzinho, xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], amapa.pracuuba.tartarugalzinho[order(year)], xlim=range(year), ylim=range(amapa.pracuuba.tartarugalzinho), pch=16) # linha entre pontos
#mtext("(a)", side=3, adj=0.05, line=-1.3)
mtext("Buffalo herd sizes at Amapa, Pracuuba and Tartarugalzinho (IBGE)", side=3, line=1, cex=0.8)

# amapa.pracuuba
plot(year, amapa.pracuuba, ylim=range(c(0, 60000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], amapa.pracuuba[order(year)], xlim=range(year), ylim=range(amapa.pracuuba), pch=16) # linha entre pontos
#mtext("(a)", side=3, adj=0.05, line=-1.3)
mtext("Efetivo de rebanho bubalino Amapa and Pracuuba (IBGE)", side=3, line=1, cex=0.8)

# cutias.tartarugalzinho
plot(year, cutias.tartarugalzinho, ylim=range(c(0, 140000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
lines(year[order(year)], cutias.tartarugalzinho[order(year)], xlim=range(year), ylim=range(cutias.tartarugalzinho), pch=16) # linha entre pontos
#mtext("(a)", side=3, adj=0.05, line=-1.3)
mtext("Efetivo de rebanho bubalino Cutias and Tartarugalzinho (IBGE)", side=3, line=1, cex=0.8)

  # check difference between 2013 and 2017 at Amapa+Pracuba
  amapa.pracuuba[11]-amapa.pracuuba[7]
  # check difference between 2013 and 2017 at Cutias+Tartarugalzinho
  cutias.tartarugalzinho[11]-cutias.tartarugalzinho[7]
  # check difference between 2013 and 2017 at LPBR
  popPIRATUBA[3]-popPIRATUBA[2]

  # Piratuba AND amapa.pracuuba
  plot(year, amapa.pracuuba, ylim=range(c(0, 60000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
  axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
  lines(year[order(year)], amapa.pracuuba[order(year)], xlim=range(year), ylim=range(amapa.pracuuba), pch=16) # linha entre pontos
  lines(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=16, lty=2) # linha entre pontos
  points(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=2, cex=0.5)
  arrows(ano, popPIRATUBA-sdevPIRATUBA, ano, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
  # Add a legend
  mtext("Piratuba", side=3, adj=0.05, line=-10, at=2016, cex=0.7)
  mtext("Amapá+Pracuúba", side=3, adj=0.05, line=-4, at=2015.3, cex=0.7)
  #mtext("Buffalo heads at LPBR (dashed) and in Amapa+Pracuuba (continuous)", side=3, line=1, cex=0.8)


    
  # Piratuba AND Pracuuba
  plot(year, pracuuba, ylim=range(c(0, 60000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1)
  axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
  lines(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=16) # linha entre pontos
  lines(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=16, lty=2) # linha entre pontos
  points(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=2, cex=0.5)
  arrows(ano, popPIRATUBA-sdevPIRATUBA, ano, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
  # Add a legend
  #mtext("(a)", side=3, adj=0.05, line=-1.3)
  mtext("Buffalo heads at Amapa and Pracuuba (IBGE) and LPBR", side=3, line=1, cex=0.8)
  

  # Piratuba AND Amapá AND Pracuuba and Tartarugalzinho
  plot(year, amapa, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, cex=0.5,  ylab="", xlab="", cex.axis=0.8, las=1, alpha=0.5)
  axis(1, seq(2007,2017,2), cex.axis=0.8, font=1, las=1)
  lines(year[order(year)], amapa[order(year)], xlim=range(year), ylim=range(amapa), pch=16, alpha=0.2) # linha entre pontos
  lines(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=16) # linha entre pontos
  lines(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=16) # linha entre pontos
  lines(year[order(year)], popPIRATUBA[order(year)], xlim=range(year), ylim=range(popPIRATUBA), pch=16, lty=2) # linha entre pontos
  points(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=2, cex=0.5, alpha=0.2)
  points(year[order(year)], tartarugalzinho[order(year)], xlim=range(year), ylim=range(tartarugalzinho), pch=2, cex=0.5, alpha=0.2)
  points(year[order(year)], popPIRATUBA[order(year)], xlim=range(year), ylim=range(popPIRATUBA), pch=2, cex=0.5)
  arrows(year, popPIRATUBA-sdevPIRATUBA, year, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
  # Add a legend
  #mtext("(a)", side=3, adj=0.05, line=-1.3)
  mtext("Buffalo heads at Amapa and Pracuuba (IBGE) and LPBR", side=3, line=1, cex=0.8)
  
  
    
# overall Y label
mtext("Efetivo de rebanhos (IBGE)", outer=T, side=2)

#---------------------
#---- Multipanel plot

par(mfrow = c(3, 1), tcl=-0.5)
par(mai = c(0.2,0.2,0.2,0.2), mar = c(0.5,0.5,0.5,0.5), oma = c(2, 6, 2, 2)) # edit oma to get space for species names
#par(mai = c(0.1,1,0.1,0.1))

# ESEC Maraca
plot(ano, popMARACA, ylim=range(c(0, 1200)), xlim=range(c(2007,2017)) , xaxt="n", yaxt="n", pch=19, xlab="Ano", ylab="População de búfalos (média +/- SD)",
     main="", cex.axis=1, las=1)
axis(2, seq(0,1200,400), cex.axis=1, font=1, las=1)
arrows(ano, popMARACA-sdevMARACA, ano, popMARACA+sdevMARACA, length=0.05, angle=90, code=3) # barra de SD
lines(ano[order(ano)], popMARACA[order(ano)], xlim=range(ano), ylim=range(popMARACA), pch=16) # linha entre pontos
mtext("(a)", side=3, adj=0.05, line=-1.3, cex=0.8)

# REBIO Piratuba
plot(ano, popPIRATUBA, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", yaxt ="n", pch=19, xlab="Ano", ylab="População de búfalos (média +/- SD)",
     main="", cex.axis=1, las=1)
axis(2, seq(0,50000,20000), cex.axis=1, font=1, las=1)
arrows(ano, popPIRATUBA-sdevPIRATUBA, ano, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
lines(ano[order(ano)], popPIRATUBA[order(ano)], xlim=range(ano), ylim=range(popPIRATUBA), pch=16) # linha entre pontos
mtext("(b)", side=3, adj=0.05, line=-1.3, cex=0.8)

# amapa.pracuuba
plot(year, amapa.pracuuba, ylim=range(c(0, 60000)), xlim=range(c(2007,2017)) , xaxt = "n", yaxt = "n",pch=19, cex=0.5,  ylab="", xlab="", cex.axis=1, las=1)
axis(1, seq(2007,2017,2), cex.axis=1, font=1, las=1)
axis(2, seq(0,60000,20000), cex.axis=1, font=1, las=1)
lines(year[order(year)], amapa.pracuuba[order(year)], xlim=range(year), ylim=range(amapa.pracuuba), pch=16) # linha entre pontos
#lines(year[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=16) # linha entre pontos
#points(years[order(year)], pracuuba[order(year)], xlim=range(year), ylim=range(pracuuba), pch=2, cex=0.5)
#lines(year[order(year)], amapa[order(year)], xlim=range(year), ylim=range(amapa), pch=16) # linha entre pontos
#points(years[order(year)], amapa[order(year)], xlim=range(year), ylim=range(amapa), pch=2, cex=0.5)
mtext("(c)", side=3, adj=0.05, line=-1.3, cex=0.8)


#------------------------

#------------------------

#-------------Estimativa por transecto, SD inter-transectos-----------------

piratuba <- buffalo[ which(buffalo$setor =="Piratuba_ Araguari"| buffalo$setor=="Piratuba_central"| buffalo$setor=="Piratuba_Noroeste"), ]
x.piratuba <- cbind.data.frame(sort(unique(piratuba$transecto)), rep(NA, length(unique(piratuba$transecto))), rep(NA, length(unique(piratuba$transecto))), rep(NA, length(unique(piratuba$transecto))) )
colnames(x.piratuba) <- c("transecto", "D", "P.detec", "cover")

piratuba$cover <- piratuba$ambiente
unique(piratuba$cover)
levels(piratuba$cover)[levels(piratuba$cover)=="a"] <- NA
levels(piratuba$cover)[levels(piratuba$cover)=="ac"] <- NA
levels(piratuba$cover)[levels(piratuba$cover)=="ca"] <- NA
levels(piratuba$cover)[levels(piratuba$cover)=="am"] <- NA
levels(piratuba$cover)[levels(piratuba$cover)=="c"] <- 1
levels(piratuba$cover)[levels(piratuba$cover)=="cm"] <- 2
levels(piratuba$cover)[levels(piratuba$cover)=="m"] <- 3
piratuba$cover <- as.numeric(levels(piratuba$cover)[piratuba$cover])


maraca <- buffalo[ which(buffalo$setor =="Maraca_sul"), ]
x.maraca <- cbind.data.frame(sort(unique(maraca$transecto)), rep(NA, length(unique(maraca$transecto))) )
colnames(x.maraca) <- c("transecto", "D")

#---------------- Preenchendo para Piratuba------------------------------

for(i in 1:nrow(x.piratuba))    # criando contador
  {
  sub.x.piratuba <- subset(piratuba, piratuba$transecto == x.piratuba[i,1])
  W <- as.numeric(sub.x.piratuba$Altitude)*6/2 # largura de amostragem em vôo, W = H*w/h
  area.sampled.m2 <- W*as.numeric(sub.x.piratuba$Compriment) # área amostrada (m²)
  area.sampled.total <- sum(na.omit(area.sampled.m2))*10^-6 # area amostrada em km2
  B <- sum(as.numeric(sub.x.piratuba$B)) # n groups seen by both observers
  S1 <- sum(as.numeric(sub.x.piratuba$S1)) # n groups seen by observer 1
  S2 <- sum(as.numeric(sub.x.piratuba$S2)) # n groups seen by observer 2
  P1 <- sum(sub.x.piratuba$B)/(sum(sub.x.piratuba$B)+sum(sub.x.piratuba$S2)) # detection probability
  P2 <- sum(sub.x.piratuba$B)/(sum(sub.x.piratuba$B)+sum(sub.x.piratuba$S1))
  M <- S1*S2/B
  y.1 <- (B+S1+1)
  y.2 <- (B+S2+1)
  y.3 <- (B+1)
  Y <- (y.1*y.2/y.3)-1
  mean.group.size <- mean(sub.x.piratuba$grupo_max[sub.x.piratuba$grupo_max!=0]) # mean group size ex.piratubacluding zeros
  pop.estimate <- Y*mean.group.size # total population in sector
  pop.density <- pop.estimate/area.sampled.total
  x.piratuba[i,2] <- pop.density
  x.piratuba[i,3] <- (P1+P2)/2
  x.piratuba[i,4] <- mean(sub.x.piratuba$cover)
  }

View(x.piratuba)

with(x.piratuba, table(P.detec, cover))

# plotting it
plot(jitter(x.piratuba$cover,2), x.piratuba$P.detec, pch=20, col=1, cex=1, main="Probabilidade de detecção", xlab="Índice de cobertura de vegetação (média por transecto", ylab="Probabilidade de detecção")

plot(jitter(x.piratuba$cover,2), x.piratuba$P.detec, ylim=c(0.5,1), pch=20, col=1, cex=1, main="Probabilidade de detecção", xlab="Índice de cobertura de vegetação (média por transecto", ylab="Probabilidade de detecção")

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

x.piratuba[is.na(x.piratuba)] <- 0 # transectos com zero buffalos estavam como NA, corrigir isso
piratuba.mean.density <- mean(x.piratuba$D)
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
  
  
