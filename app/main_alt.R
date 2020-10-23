mydf <- Dati
df <- data.frame(X1 = rep(0), X2 = rep(0), X3 = rep(0),
                 X4 = (0), X5 = rep(0), X6 = rep(0))
for (i in 1:20){
  for (j in 2:7) {
    mydf[i, j] <- as.integer(mydf[i, j])
    df[i, j-1] <- mydf[i, j]
  }
}
remove(i, j)

### MAIN ###

  # Pareto section
  createPareto("Raccolta Indifferenziata (Pareto)", datiRaccoltaIndifferenziata)
  
  # Getting Sicily
  SicilyValue = datiRaccoltaIndifferenziata[19]
  waste = SicilyValue - mean(datiRaccoltaIndifferenziata)
  waste # result is 1143139
  
  umido = mydf$`Rifiuti organici`
  cartone = mydf$`Carta e cartone`
  umido = umido / 1000
  cartone = cartone / 1000
  
  plot(cartone, umido, main = "Rifiuti organici in funzione del cartone",
      xlab = "Cartone", ylab = "Organico", col = "red")
  abline(v=median(cartone), lty=1, col="magenta")
  abline(v=mean(cartone), lty=2, col="blue")
  abline(h=median(umido), lty=1, col="magenta")
  abline(h=mean(umido), lty=2, col="blue")
  legend (50, 1200, c("Mediana", "Media"), pch =0, col=c("magenta", "blue"),
          cex =0.8)
  
  median(cartone)
  mean(cartone)
  median(umido)
  mean(umido)
  
  cov(cartone, umido)
  cor(cartone, umido)
  
  plot(cartone, umido, main = "Rifiuti organici in funzione del cartone",
       xlab = "Cartone", ylab = "Organico", col = "red")
  abline(v=median(cartone), lty=1, col="magenta")
  abline(v=mean(cartone), lty=2, col="blue")
  abline(h=median(umido), lty=1, col="magenta")
  abline(h=mean(umido), lty=2, col="blue")
  legend (50, 1200, c("Mediana", "Media"), pch =0, col=c("magenta", "blue"),
          cex =0.8)
  abline(lm(umido~cartone), col="blue")
  
  beta <- (sd(umido) / sd(cartone)) * cor(cartone, umido)
  alpha <- mean(umido) - beta*mean(cartone)
  c(alpha, beta)
  
  
  plot(cartone, umido, main = "Rifiuti organici in funzione del cartone",
       xlab = "Cartone", ylab = "Organico", col = "red")
  abline(lm(umido~cartone), col="blue")
  stime<-fitted(lm(umido~cartone))
  segments(cartone, stime, cartone, umido, col="magenta")
  
  residui <- resid(lm(umido~cartone))
  plot(cartone, residui, main = "Diagramma dei residui",
       xlab = "Cartone", ylab = "Residui", col = "red", pch=9)
  abline(h=0, col="blue", lty=2)
  
  summary (lm(umido~cartone))$r.square
  
  toAnalyze = data.frame(mydf$`Rifiuti organici`, mydf$`Carta e cartone`,
                         mydf$Vetro, mydf$Plastica, mydf$Altro)
  toAnalyze = toAnalyze / 1000
  pairs(toAnalyze)
  
  cov(toAnalyze)
  cor(toAnalyze)
  
  lm(toAnalyze$mydf..Rifiuti.organici.~toAnalyze$mydf..Carta.e.cartone.+
       toAnalyze$mydf.Vetro+toAnalyze$mydf.Plastica+toAnalyze$mydf.Altro)
  
  stime = fitted(lm(toAnalyze$mydf..Rifiuti.organici.~
                      toAnalyze$mydf..Carta.e.cartone.+toAnalyze$mydf.Vetro+
                      toAnalyze$mydf.Plastica+toAnalyze$mydf.Altro))
  residui = resid(lm(toAnalyze$mydf..Rifiuti.organici.~
                       toAnalyze$mydf..Carta.e.cartone.+toAnalyze$mydf.Vetro+
                       toAnalyze$mydf.Plastica+toAnalyze$mydf.Altro))
  stime
  residui
  
  
  