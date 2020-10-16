mydf <- Dati
for (i in 1:20){
  for (j in 2:7) {
    mydf[i, j] <- as.integer(mydf[i, j])
  }
}
remove(i, j)

  ### MAIN ###
  
  # Getting the row arrays
  datiRaccoltaIndifferenziata = mydf$`Raccolta indifferenziata`
  datiCarta = mydf$`Carta e cartone`
  datiPlastica = mydf$Plastica
  datiVetro = mydf$Vetro
  datiUmido = mydf$`Rifiuti organici`
  datiAltro = mydf$Altro
  
  # ATTENTION
    # Before continuing, re-group all graphs in only one environment
  
  createBarPlot_limit("Carta e Cartone", datiCarta, 1000000)
  createBarPlot_limit("Plastica", datiPlastica, 1000000)
  createBarPlot_limit("Vetro", datiVetro, 1000000)
  createBarPlot_limit("Carta e Cartone", datiCarta, 1000000)
  createBarPlot_limit("Carta e Cartone", datiCarta, 1000000)
  
  # OR
    # Jump chapter 2 and start another chapter by ignoring previous code