
# Função para estimar população de búfalos em cada setor da REBIO Piratuba

double.count <- function(x, k, l, z) # argumentos: x=setor da reserva, k e l = tamanho minimo e maximo de grupo, z = área do setor
{
  area.sector <- z
  buf <- x
  buf <- subset(buf, grupo_max==0 | grupo_max >= k)
  buf <- subset(buf, grupo_max <= l)
  assign("buf", buf, .GlobalEnv)
  W <- 300 #as.numeric(buf$Altitude)*6/2 # largura de amostragem em vôo, W = H*w/h
  area.sampled.m2 <- W*as.numeric(buf$Compriment) # área amostrada (m²)
  area.sampled.km2 <- sum(na.omit(area.sampled.m2))*10^-6 # area amostrada em km2
  #area.sampled.km2 <- 0.45*nrow(buf)
  B <- sum(as.numeric(buf$B)) # n groups seen by both observers
  S1 <- sum(as.numeric(buf$S1)) # n groups seen by observer 1
  S2 <- sum(as.numeric(buf$S2)) # n groups seen by observer 2
  P1 <- sum(buf$B)/(sum(buf$B)+sum(buf$S2)) # detection probability
  P2 <- sum(buf$B)/(sum(buf$B)+sum(buf$S1))
  M <- S1*S2/B
  y.1 <- (B+S1+1)
  y.2 <- (B+S2+1)
  y.3 <- (B+1)
  Y <- ((y.1*y.2)/(y.3))-1 #Y <- (B+S1+1)(B+S2+1) / (B+1) - 1 # estimate of the size of the population (of groups)
  # var.Y <- (S1*S2(B+S1+1)(B+S2+1))/((B+1)^2(B+2)) # variance of Y
  mean.group.size <- mean(buf$grupo_max[buf$grupo_max!=0]) # mean group size excluding zeros
  pop.estimate <- Y*mean.group.size # total population in sector
  pop.density <- pop.estimate/area.sampled.km2
  total <-pop.density*area.sector
  assign("total", total, .GlobalEnv) # valor reservado para uso pela função ci.boot
  ret <- list("População de búfalos" = total, "Densidade (ind/km2)" = pop.density, "Tamanho médio de grupo" = mean.group.size)
  return(ret)
}

# Função para calcular intervalo de confiança da estimativa populacional

ci.count <- function(x,k,l,z) # x = setor da reserva, y=dados
{
  bux <- x
  buf$iboot <- 1:nrow(buf) # criando coluna índice
  boot.ci <- rep(NA, 10000)    # criando objeto para receber valores de simulação
  for(i in 1:10000)    # criando contador
  {
  iboot <- sample(1:nrow(buf), replace=TRUE) # gerando vetor de números aleatórios
  bootdata <- buf[buf$iboot %in% iboot,] # bootdata = subset do buf somente linhas iguais aos do vetor iboot criado acima
  double.count(bootdata,k,l,z)
  boot.ci[i] <- total  # preenchendo objeto
  assign("boot.ci", boot.ci, .GlobalEnv)
  }
  a <- round(quantile(boot.ci, probs=c(0.025, 0.975), na.rm=T), 4)    # criando objeto com quantis referentes ao 95% CI
  b <- data.frame(matrix(c(a), ncol = 2, nrow = 1, byrow = TRUE))    # criando objeto com resultados da analise
  colnames(b) <- c("Q 2.5%", "Q 97.5%")    # modificando nomes de colunas de b
  c <- sd(boot.ci, na.rm=T) # SD da estimativa
  ret2 <- list("IC 95%" = b, "sd" = c)
  return(ret2)
}
