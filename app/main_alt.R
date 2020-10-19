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
  
  