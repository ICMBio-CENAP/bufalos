
# Analysis code
# Feral buffaloes at Piratuba Biological Reserve and Maracá-Jipioca Ecological Station 
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

### Part 2. Explore results

##----- 1 - Load libraries-----
library(here)
#library(ggplot2)

##----- 2 - Source files-----
source(here("bin", "multiplot.R")) # using package here to build a path to the subdirectory "bin" within "jamari"


##-----3 - Read data -----
#piratuba <- read.csv(here("data", "piratuba.csv"))
#maraca <- read.csv(here("data", "maraca.csv"))


#------------------------------------------------
# Check results from Tomas et al. 2011 for comparative purposes:
# mean density Piratuba = 19,6 ind/km2 (nw, w and south)
# total population Piratuba = 33354 ind
# 33354/19.6 = 1701.735 km2 = 170173 ha
# Piratuba has 392469 ha, so the population must have been estimated for 43% of the PA (my estimate is for 59% of the PA)

# noroeste/oeste density = 9 ind/km2
# noroeste/oeste population = 11759 ind
# 11759/9 = 1306.5 km2 = 130650 ha (my estimate is for 93187 ha)

# araguari density = 51 ind/km2
# araguari population = 20703 ind
# 20703/51 = 405.9 km2 = 40000 ha (my estimate is for 138926 ha)
#------------------------------------------------


##-----3 - Plot temporal trends -----

# Bundle Tomas et al. estimates with novel estimate:
popPIRATUBA <- c(33354, 31887, 17782)
popMARACA <- c(641,528,691.73)
sdevPIRATUBA <- c(4680, 3442, 2281.961)
sdevMARACA <- c(235,187,329.9)
year <- c(2007, 2013, 2017)

# Bundle trends from IBGE data (efetivo rebanho bubalino IBGE):
# source: https://cidades.ibge.gov.br/brasil/ap/tartarugalzinho/pesquisa/18/16459?indicador=16535&ano=2017&localidade1=160021&localidade2=160010
yearIBGE <- c(2007:2017)
tartarugalzinho <- c(25981, 26798, 24705, 26987, 28174, 30177, 31625, 33396, 42328, 45711, 47195)
pracuuba <- c(15349, 14931, 15707, 16348, 18197, 19710, 20813, 20020, 12129, 11108, 8790)
amapa <- c(32579, 30714, 29117, 30453, 32456, 34191, 35729, 37465, 34177, 32155, 33793)
cutias <- c(48753, 49815, 51195, 53641, 62548, 65970, 69796, 72936, 75282, 76980, 79113)


# Plot trends for REBIO Piratuba
plot(year, popPIRATUBA, ylim=range(c(0, 40000)), xlim=range(c(2007,2017)) , xaxt = "n", pch=19, xlab="year", ylab="População de búfalos (média +/- SD)",
	 main="População de búfalos na REBIO Piratuba, 2007-2017")
axis(1, seq(2007,2017,1),las=2, cex.axis=0.8, font=2)
arrows(year, popPIRATUBA-sdevPIRATUBA, year, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
lines(year[order(year)], popPIRATUBA[order(year)], xlim=range(year), ylim=range(popPIRATUBA), pch=16) # linha entre pontos

# Plot trends for ESEC Maraca
plot(year, popMARACA, ylim=range(c(0, 1000)), xlim=range(c(2007,2017)) , xaxt="n", pch=19, xlab="year", ylab="População de búfalos (média +/- SD)",
	 main="População de búfalos na ESEC Maracá-Jipioca, 2008-2017")
axis(1, seq(2007,2017,1),las=2, cex.axis=0.8, font=2)
arrows(year, popMARACA-sdevMARACA, year, popMARACA+sdevMARACA, length=0.05, angle=90, code=3) # barra de SD
lines(year[order(year)], popMARACA[order(year)], xlim=range(year), ylim=range(popMARACA), pch=16) # linha entre pontos


#-----------
# Make multiplot for trends (Fig 2 for the paper)

# write figure as a function
fig2 <- function(){
	par(mfrow = c(3, 1), tcl=-0.5)
	par(mai = c(0.2,0.2,0.2,0.2), mar = c(0.5,0.5,0.5,0.5), oma = c(2, 6, 2, 2)) # edit oma to get space for species names
	# ESEC Maraca
	plot(year, popMARACA, ylim=range(c(0, 1200)), xlim=range(c(2007,2017)) , xaxt="n", yaxt="n", pch=19, xlab="year", ylab="População de búfalos (média +/- SD)",
	 main="", cex.axis=1, las=1)
	axis(2, seq(0,1200,400), cex.axis=1.5, font=1, las=1)
	arrows(year, popMARACA-sdevMARACA, year, popMARACA+sdevMARACA, length=0.05, angle=90, code=3) # barra de SD
	lines(year[order(year)], popMARACA[order(year)], xlim=range(year), ylim=range(popMARACA), pch=16) # linha entre pontos
	mtext("(A)", side=3, adj=0.05, line=-2.5, cex=1.5)
	
	# REBIO Piratuba
	plot(year, popPIRATUBA, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", yaxt ="n", pch=19, xlab="year", ylab="População de búfalos (média +/- SD)",
	 main="", cex.axis=1, las=1)
	axis(2, seq(0,50000,20000), cex.axis=1.5, font=1, las=1)
	arrows(year, popPIRATUBA-sdevPIRATUBA, year, popPIRATUBA+sdevPIRATUBA, length=0.05, angle=90, code=3) # barra de SD
	lines(year[order(year)], popPIRATUBA[order(year)], xlim=range(year), ylim=range(popPIRATUBA), pch=16) # linha entre pontos
	mtext("(B)", side=3, adj=0.05, line=-2.5, cex=1.5)
	
	# amapa AND pracuuba and tartarugalzinho
	plot(yearIBGE, amapa, ylim=range(c(0, 50000)), xlim=range(c(2007,2017)) , xaxt = "n", yaxt ="n", pch=2, cex=0.8,  ylab="", xlab="", cex.axis=0.8, las=1)
	axis(1, seq(2007,2017,2), cex.axis=1.5, font=1, las=1)
	axis(2, seq(0,50000,15000), cex.axis=1.5, font=1, las=1)
	lines(yearIBGE[order(yearIBGE)], amapa[order(yearIBGE)], xlim=range(yearIBGE), ylim=range(amapa), pch=16, lty=3) # linha entre pontos
	points(yearIBGE[order(yearIBGE)], pracuuba[order(yearIBGE)], xlim=range(yearIBGE), ylim=range(pracuuba), pch=20, cex=0.8)
	lines(yearIBGE[order(yearIBGE)], pracuuba[order(yearIBGE)], xlim=range(yearIBGE), ylim=range(pracuuba), pch=16, lty=1) # linha entre pontos
	points(yearIBGE[order(yearIBGE)], tartarugalzinho[order(yearIBGE)], xlim=range(yearIBGE), ylim=range(tartarugalzinho), pch=0, cex=0.8)
	lines(yearIBGE[order(yearIBGE)], tartarugalzinho[order(yearIBGE)], xlim=range(yearIBGE), ylim=range(tartarugalzinho), pch=16, lty=5) # linha entre pontos
	#legend(2007, 15000, legend=c("Amapa", "Pracuuba", "Tartarugalzinho"), pch=c(2,20,0), lty=c(3,1,5),  cex=0.8, box.lty=0)
	mtext("(C)", side=3, adj=0.05, line=-2.5, cex=1.5)
	text(c(2016.5,2016.5,2016.7),c(49695,36293,11290),labels=c("Tartarugalzinho", "Amapá","Pracuuba"), cex=1.5)
	#text(c(2016.8,2016.8,2016.8),c(43195,29793,4790),labels=c("Tartarugalzinho", "Amapá","Pracuuba"), cex=1.5)
	}

# run it:
fig2()

# save as jpeg
jpeg(here("results", "Fig2.jpg"), width = 700, height = 900) # Open jpeg file
fig2()
dev.off()

# save as tiff
tiff(here("results", "Figure-2.tiff"), width = 2600, height = 3200, res=300) # Open tiff file
fig2()
dev.off()


#------------------------------------

# check difference between 2013 and 2017 at Amapa+Pracuba
(amapa[11]+pracuuba[11])-(amapa[7]+pracuuba[7])

# check difference between 2013 and 2017 at Cutias+Tartarugalzinho
(cutias[11]+tartarugalzinho[11])-(cutias[7]+tartarugalzinho[7])

# check difference between 2013 and 2017 at LPBR
popPIRATUBA[3]-popPIRATUBA[2]


