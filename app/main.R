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
  namesRegioni <- c("Piemonte","Valle d'Aosta /Vallée d'Aoste","Liguria",
                    "Lombardia","Trentino-Alto Adige/Südtirol","Veneto",
                    "Friuli-Venezia Giulia","Emilia-Romagna","Toscana",
                    "Umbria","Marche","Lazio","Abruzzo","Molise","Campania",
                    "Puglia","Basilicata","Calabria","Sicilia","Sardegna")
  datiRaccoltaIndifferenziata = mydf$`Raccolta indifferenziata`
  datiRaccoltaIndifferenziata
  datiCarta = mydf$`Carta e cartone`
  datiCarta
  datiPlastica = mydf$Plastica
  datiPlastica
  datiVetro = mydf$Vetro
  datiVetro
  datiUmido = mydf$`Rifiuti organici`
  datiUmido
  datiAltro = mydf$Altro
  datiAltro
  
  # ATTENTION
    # Before continuing, re-group all graphs in only one environment
  createBarPlot_limit("Rifiuti Indifferenziata", 
                      datiRaccoltaIndifferenziata, 1.05, 1)
  createBarPlot_limit("Rifiuti organici", datiUmido, 1.05, 1)
  createBarPlot_limit("Carta e Cartone", datiCarta, 1.05, 1)
  createBarPlot_limit("Vetro", datiVetro, 1.05, 1)
  createBarPlot_limit("Plastica", datiPlastica, 1.05, 1)
  createBarPlot_limit("Altro", datiAltro, 1.05, 1)
  
  #Display Pie with garbage for region
  
  createPie("Rifiuti Indifferenziata",datiRaccoltaIndifferenziata)
  createPie("Rifiuti organici",datiUmido)
  createPie("Carta e cartone",datiCarta)
  createPie("Vetro",datiVetro)
  createPie("Plastica",datiPlastica)
  createPie("Altro",datiAltro)
  
  # OR
    # Jump chapter 2 and start another chapter by ignoring previous code
  
  #barplot
  createBarPlot_multiple(df)
  
  #histogram with garbage for region
  createHisto("Istogramma Rifiuti Indifferenziata",datiRaccoltaIndifferenziata)
  createHisto("Istogramma Rifiuti organici",datiUmido)
  createHisto("Istogramma Carta e cartone",datiCarta)
  createHisto("Istogramma Vetro",datiVetro)
  createHisto("Istogramma Plastica",datiPlastica)
  createHisto("Istogramma Altro",datiAltro)
  
  #quantile mean and median, we divide all for 1000
  datiRaccoltaIndifferenziata2<-datiRaccoltaIndifferenziata/1000
  datiRaccoltaIndifferenziata2<-as.integer(datiRaccoltaIndifferenziata2)
  datiUmido2<-datiUmido/1000
  datiUmido2<-as.integer(datiUmido2)
  datiCarta2<-datiCarta/1000
  datiCarta2<-as.integer(datiCarta2)
  datiVetro2<-datiVetro/1000
  datiVetro2<-as.integer(datiVetro2)
  datiPlastica2<-datiPlastica/1000
  datiPlastica2<-as.integer(datiPlastica2)
  datiAltro2<-datiAltro/1000
  datiAltro2<-as.integer(datiAltro2)
  
  quantile(datiRaccoltaIndifferenziata2)
  quantile(datiUmido2)
  quantile(datiCarta2)
  quantile(datiVetro2)
  quantile(datiPlastica2)
  quantile(datiAltro2)
  
  summary(datiRaccoltaIndifferenziata2)  
  summsary(datiUmido2)
  summary(datiCarta2)
  summary(datiVetro2)
  summary(datiPlastica2)
  summary(datiAltro2)
  
  #Display BoxPlot with garbage for region /1000
  createBoxPlot_base("Rifiuti Indifferenziata",datiRaccoltaIndifferenziata2)
  createBoxPlot_base("Rifiuti organici",datiUmido2)
  createBoxPlot_base("Carta e cartone",datiCarta2)
  createBoxPlot_base("Vetro",datiVetro2)
  createBoxPlot_base("Plastica",datiPlastica2)
  createBoxPlot_base("Altro",datiAltro2)
  
  