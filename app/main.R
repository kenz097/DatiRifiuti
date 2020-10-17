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
  
  # Getting the row arrays

  labelsRifiuti <- c("Raccolta Indifferenziata","Rifiuti organici",
                     "Carta e cartone","Vetro","Plastica","Altro")
  datiRaccoltaIndifferenziata = mydf$`Raccolta indifferenziata`
  datiCarta = mydf$`Carta e cartone`
  datiPlastica = mydf$Plastica
  datiVetro = mydf$Vetro
  datiUmido = mydf$`Rifiuti organici`
  datiAltro = mydf$Altro
  
  # ATTENTION
    # Before continuing, re-group all graphs in only one environment
  
  createBarPlot_limit("Carta e Cartone", datiCarta, 1.05, 1)
  createBarPlot_limit("Plastica", datiPlastica, 1.05, 1)
  createBarPlot_limit("Vetro", datiVetro, 1.05, 1)
  createBarPlot_limit("Organici", datiUmido, 1.05, 1)
  createBarPlot_limit("Altro", datiAltro, 1.05, 1)
  createBarPlot_limit("Rifiuti Indifferenziata", 
                      datiRaccoltaIndifferenziata, 1.05, 1)
  
  #Display Pie with garbage for region
  
  createPie("Rifiuti Indifferenziata",datiRaccoltaIndifferenziata)
  createPie("Rifiuti Organici",datiUmido)
  createPie("Carta e cartone",datiCarta)
  createPie("Vetro",datiVetro)
  createPie("Plastica",datiPlastica)
  createPie("Altro",datiAltro)
  
  # OR
    # Jump chapter 2 and start another chapter by ignoring previous code
  
  df
  #barplot
  createBarPlot_congiunta(df)
  
  