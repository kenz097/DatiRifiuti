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
  
  
  
  # chap 7

  set <- scale(df)
  row.names(set) <- namesRegioni
  d <- dist(set);
  hlsSingle <-hclust (d, method = "single");
  str(hlsSingle);
  
  plot(hlsSingle, hang =-1,
       xlab="Metodo del legame singolo")
  
  hlsComplete <-hclust (d, method = "complete");
  str(hlsComplete);
  
  plot(hlsComplete, hang =-1,
       xlab="Metodo del legame completo")
  
  hlsMedian <-hclust (d, method = "median");
  str(hlsMedian);
  
  plot(hlsMedian, hang =-1,
       xlab="Metodo del legame medio")
  
  hlsCentroid <-hclust (d^2, method = "centroid");
  str(hlsCentroid);
  
  plot(hlsCentroid, hang =-1,
       xlab="Metodo del centroide")
  
  hlsMedian <-hclust (d^2, method = "median");
  str(hlsMedian);
  
  plot(hlsMedian, hang =-1,
       xlab="Metodo della mediana")
  
  #partitions by means of rectangles
  plot(hlsCentroid, hang =-1,
       xlab="Metodo del centroide")
  axis(side=4,at=round(c(0,hlsCentroid$height),2))
  rect.hclust(hlsCentroid,k=2,border="red")
  rect.hclust(hlsCentroid,k=3,border="green")
  
  #from float to int
  arrotondato<-round(toAnalyze,0)
  arrotondato
  
  #see how regions are classified as the number of clusters increases
  cutree(hlsCentroid,k=1:20)
  
  #mean and median and sample standard deviation
  cutT<-cutree(hlsCentroid,k=3,h=NULL)
  listCut<-list(cutT)
  
  #use method aggregate
  aggregate(arrotondato,listCut,mean)
  aggregate(arrotondato,listCut,var)
  aggregate(arrotondato,listCut,sd)
  
  #Non-hierarchical methods
  km<-kmeans(arrotondato,center=3,iter.max=10,nstart=1)
  km
  str(km)
  
  #now we try to scale data and use kmeans
  Z<-scale(arrotondato)
  Z
  km1<-kmeans(Z,center=3,iter.max = 10,nstart=10)
  km1
  
  
  
  