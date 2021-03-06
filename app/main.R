mydf <- Dati
df <- data.frame(X1 = rep(0), X2 = rep(0), X3 = rep(0),
                 X4 = (0), X5 = rep(0), X6 = rep(0))
for (i in 1:20){
  for (j in 2:7) {
    mydf[i, j] <- as.integer(mydf[i, j]+0.5)
    df[i, j-1] <- mydf[i, j]
  }
}
remove(i, j)

  ### MAIN ###
  
  # Getting the row arrays
  mydf
  df
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
  datiRaccoltaIndifferenziata2<-as.integer(datiRaccoltaIndifferenziata2+0.5)
  datiUmido2<-datiUmido/1000
  datiUmido2<-as.integer(datiUmido2+0.5)
  datiCarta2<-datiCarta/1000
  datiCarta2<-as.integer(datiCarta2+0.5)
  datiVetro2<-datiVetro/1000
  datiVetro2<-as.integer(datiVetro2+0.5)
  datiPlastica2<-datiPlastica/1000
  datiPlastica2<-as.integer(datiPlastica2+0.5)
  datiAltro2<-datiAltro/1000
  datiAltro2<-as.integer(datiAltro2+0.5)
  
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

  #Euclidean distance, we use only inside R NOT IN THE WORD
  distance<-round(df/1000,0)
  rownames(distance)<-namesRegion()
  colnames(distance)<-namesGarbage()
  dist(distance,method="euclidean",diag=TRUE,upper=TRUE)
  #scale and standardize data
  d<-scale(distance)
  #now we calculate again the data but by scale and standardize data
  dist(d,method="euclidean",diag=TRUE,upper=TRUE)
  
  # chap 7, continuation of the cluster analysis
  toAnalyze = data.frame(mydf$`Rifiuti organici`, mydf$`Carta e cartone`,
                         mydf$Vetro, mydf$Plastica, mydf$Altro)
  toAnalyze = toAnalyze / 1000
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
  
  hlsAvarage<-hclust (d, method = "average");
  str(hlsAvarage);
  
  plot(hlsAvarage, hang =-1,
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
  #Centroid
  cutT<-cutree(hlsCentroid,k=3,h=NULL)
  listCut<-list(cutT)
  #single
  cutT1<-cutree(hlsSingle,k=3,h=NULL)
  listCut1<-list(cutT1)
  #completed
  cutT2<-cutree(hlsComplete,k=3,h=NULL)
  listCut2<-list(cutT2)
  #median
  cutT3<-cutree(hlsMedian,k=3,h=NULL)
  listCut3<-list(cutT3)
  #average
  cutT4<-cutree(hlsAvarage,k=3,h=NULL)
  listCut4<-list(cutT4)
  #dendrogram
  
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
  str(km1)
  
  #relative internal sum non non-homogeneity between cluster 
  km1$tot.withinss/km1$totss
  #relative non-homogeneity between cluster 
  km1$betweenss/km1$totss
  
  #for do this we can't write all code but we only write d and after come here
  covMatrix<-cov(d)
  covMatrix  
  
  #non-homogeneity matrix
  NHM<-19*covMatrix
  NHM
  
  #non-homogeneity matrix of data set standardize
  NHMS<-(20-1)*sum(apply(d,2,var))
  NHMS
  #all type of division on cluster
  #centroid
  agvr<-aggregate(d,listCut,var)[,-1]
  agvr  
  #single
  agvr1<-aggregate(d,listCut1,var)[,-1]
  agvr1
  #completed
  agvr2<-aggregate(d,listCut2,var)[,-1]
  agvr2
  #median
  agvr3<-aggregate(d,listCut3,var)[,-1]
  agvr3
  #average
  agvr4<-aggregate(d,listCut4,var)[,-1]
  agvr4
  
  
  num<-table(cutT)
  num
  num1<-table(cutT2)
  num1

  #calculate again non-homogeneity matrix first
  trh1<-(num[[1]]-1)*sum(agvr[1,])
  trh1
  trh2<-(num[[2]]-1)*sum(agvr[2,])
  trh2
  trh3<-(num[[3]]-1)*sum(agvr[3,])
  trh3 #this is NA because is only Lombardia and value=
  #second matrix
  trh11<-(num1[[1]]-1)*sum(agvr[1,])
  trh11
  trh22<-(num1[[2]]-1)*sum(agvr[2,])
  trh22
  trh33<-(num1[[3]]-1)*sum(agvr[3,])
  trh33 #this is NA because is only Lombardia and value=0
  
  #internal sum non-homogeneity between cluster first(within)
  trHS<-trh1+trh2
  trHS
  #non-homogeneity between cluster first(between)
  trHB<-NHMS-trHS
  trHB
  #internal sum non-homogeneity between cluster second(within)
  trHS1<-trh11+trh22
  trHS1
  #non-homogeneity between cluster second(between)
  trHB1<-NHMS-trHS1
  trHB1
  
  
  #Relative measures first
  #within divided total non-homogeneity
  trHS/NHMS
  #between divided total non-homogeneity
  trHB/NHMS
  #second
  #within divided total non-homogeneity
  trHS1/NHMS
  #between divided total non-homogeneity
  trHB1/NHMS
  
  
  
  
  
  