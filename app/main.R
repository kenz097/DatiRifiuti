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
  datiUmido = mydf$`Rifiuti organici`
  datiUmido
  datiCarta = mydf$`Carta e cartone`
  datiCarta
  datiVetro = mydf$`Vetro`
  datiVetro
  datiPlastica = mydf$`Plastica`
  datiPlastica
  datiAltro = mydf$`Altro`
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
  summary(datiUmido2)
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
  createBoxPlot_notch("Rifiuti organici", datiUmido2)
  
  #Display diagram of discrete empirical distribution
  
  createDiscreteEmpiricalDistribution(datiRaccoltaIndifferenziata2)
  createDiscreteEmpiricalDistribution(datiUmido2)
  createDiscreteEmpiricalDistribution(datiCarta2)
  createDiscreteEmpiricalDistribution(datiVetro2)
  createDiscreteEmpiricalDistribution(datiPlastica2)
  createDiscreteEmpiricalDistribution(datiAltro2)
  
  #Display diagram of Continuous empirical distribution function
  createContinuousEmpiricalDistribution(datiRaccoltaIndifferenziata2)
  createContinuousEmpiricalDistribution(datiUmido2)
  createContinuousEmpiricalDistribution(datiCarta2)
  createContinuousEmpiricalDistribution(datiVetro2)
  createContinuousEmpiricalDistribution(datiPlastica2)
  createContinuousEmpiricalDistribution(datiAltro2)
  
  #Display Median graph divided by 1000
  createMedianGraph("Rifiuti indifferenziata",datiRaccoltaIndifferenziata2)
  createMedianGraph("Rifiuti organici",datiUmido2)
  createMedianGraph("Carta e cartone",datiCarta2)
  createMedianGraph("Vetro",datiVetro2)
  createMedianGraph("Plastica",datiPlastica2)
  createMedianGraph("Altro",datiAltro2)
  
  #different types of quantiles for types of algorithms of normale and /1000
  typesQuantiles(datiRaccoltaIndifferenziata)
  typesQuantiles(datiRaccoltaIndifferenziata2)
  typesQuantiles(datiUmido)
  typesQuantiles(datiUmido2)
  typesQuantiles(datiCarta)
  typesQuantiles(datiCarta2)
  typesQuantiles(datiVetro)
  typesQuantiles(datiVetro2)
  typesQuantiles(datiPlastica)
  typesQuantiles(datiPlastica2)
  typesQuantiles(datiAltro)
  typesQuantiles(datiAltro2)
  
  #sample variance, sample standard deviation and coefficient of variation /1000
  var(datiRaccoltaIndifferenziata2)
  sd(datiRaccoltaIndifferenziata2)
  cv(datiRaccoltaIndifferenziata2)
  var(datiUmido2)
  sd(datiUmido2)
  cv(datiUmido2)
  var(datiCarta2)
  sd(datiCarta2)
  cv(datiCarta2)
  var(datiVetro2)
  sd(datiVetro2)
  cv(datiVetro2)
  var(datiPlastica2)
  sd(datiPlastica2)
  cv(datiPlastica2)
  var(datiAltro2)
  sd(datiAltro2)
  cv(datiAltro2)
  
  #skewness /1000
  skw(datiRaccoltaIndifferenziata2)
  skw(datiUmido2)
  skw(datiCarta2)
  skw(datiVetro2)
  skw(datiPlastica2)
  skw(datiAltro2)
  
  #sample kurtosis /1000
  curt(datiRaccoltaIndifferenziata2)
  curt(datiUmido2)
  curt(datiCarta2)
  curt(datiVetro2)
  curt(datiPlastica2)
  curt(datiAltro2)
  
  
  
  