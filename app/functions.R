# namesGarbage : return names of garbage
namesGarbage<-function(){
  namesRifiuti <- c("Raccolta Indifferenziata","Rifiuti organici",
                    "Carta e cartone","Vetro","Plastica","Altro")
  return (namesRifiuti)
}

#namesRegion : rerurn names of Region
namesRegion<-function(){
  namesRegioni <- c("Piemonte","Valle d'Aosta","Liguria",
                    "Lombardia","Trentino-Alto","Veneto",
                    "Friuli-Venezia Giulia","Emilia-Romagna","Toscana",
                    "Umbria","Marche","Lazio","Abruzzo","Molise","Campania",
                    "Puglia","Basilicata","Calabria","Sicilia","Sardegna")
  return (namesRegioni)
  
}
# createPareto : displays a Pareto diagram with frequencies
##  input -> name : gives a name to the Pareto diagram
##           arrayToAnalyze : array with indexes to display
createPareto <- function (name, arrayToAnalyze) {
  
  # Calculating frequencies
  mySum = sum(arrayToAnalyze)
  arrayToAnalyze <- arrayToAnalyze / mySum
  
  # Ordering 
  ariOrdered <- order(arrayToAnalyze, decreasing = TRUE)
  
  # Creating graphic
  bp <- barplot(arrayToAnalyze[ariOrdered], main = name,
                col=rainbow(length(arrayToAnalyze)), 
                names = mydf$Regioni[ariOrdered], las=2, ylim = c(0, 1.05))
  
  lines(bp, cumsum(arrayToAnalyze[ariOrdered]), 
        type = "b", pch = 16)
  
  text(bp - 0.2, cumsum(arrayToAnalyze[ariOrdered]) + 0.03,
       paste(format(cumsum(arrayToAnalyze[ariOrdered]) * 100,
                    digits = 2), "%"))
}

# createBarPlot_base : displays a BarPlot diagram
##  input -> name : gives a name to the BarPlot diagram
##           arrayToAnalyze : array with indexes to display
##           frequency : 0 -> use indexes | 1 -> use frequences
createBarPlot_base <- function (name, arrayToAnalyze, frequency) {
  if (frequency == 1) {
    mySum = sum(arrayToAnalyze)
    arrayToAnalyze <- arrayToAnalyze / mySum
  }
  barplot(arrayToAnalyze, col=rainbow(length(arrayToAnalyze)),
          names = mydf$Regioni, las=2, main = name)
}

# createBarPlot_limit : displays a BarPlot diagram with a max limit on y axis
##  input -> name : gives a name to the BarPlot diagram
##           arrayToAnalyze : array with indexes to display
##           maxLimit : the y limit on the axis
##           frequency : 0 -> use indexes | 1 -> use frequences
createBarPlot_limit <- function (name, arrayToAnalyze, maxLimit, frequency) {
  if (frequency == 1) {
    mySum = sum(arrayToAnalyze)
    arrayToAnalyze <- arrayToAnalyze / mySum
  }
  barplot(arrayToAnalyze, col=rainbow(length(arrayToAnalyze)),
          ylim = c(0, maxLimit), names = mydf$Regioni, las=2, main = name)
}

# createBarPlot_ordered : displays a BarPlot diagram with a max limit on y axis
#                         and with ordered indexes
##  input -> name : gives a name to the BarPlot diagram
##           arrayToAnalyze : array with indexes to display
##           maxLimit : the y limit on the axis
##           frequency : 0 -> use indexes | 1 -> use frequences
createBarPlot_ordered <- function (name, arrayToAnalyze, maxLimit, frequency) {
  if (frequency == 1) {
    mySum = sum(arrayToAnalyze)
    arrayToAnalyze <- arrayToAnalyze / mySum
  }
  dataOrder <- order(arrayToAnalyze, decreasing = TRUE)
  barplot(arrayToAnalyze[dataOrder],
          col=rainbow(length(arrayToAnalyze)),
          ylim = c(0, maxLimit), main = name,
          names = mydf$Regioni[dataOrder], las=2)
}

# createBoxPlot_base : displays a BoxPlot with default settings
##  input -> name : gives a name to the BoxPlot diagram
##           arrayToAnalyze : array with indexes to display
createBoxPlot_base <- function(name, arrayToAnalyze) {
  boxplot(arrayToAnalyze, col="green", main = name)
}

# createBoxPlot_colored : displays a BoxPlot with custom color
##  input -> name : gives a name to the BoxPlot diagram
##           arrayToAnalyze : array with indexes to display
##           customColor : color of the BoxPlot
createBoxPlot_colored <- function(name, arrayToAnalyze, customColor) {
  boxplot(arrayToAnalyze, col=customColor, main = name)
}

# createBoxPlot_base : displays a Pie diagram
##  input -> name : gives a name to the Pie diagram
##           arrayToAnalyze : array with indexes to display
createPie<-function(name, arrayToAnalyze){
  # Ordering
  ariOrdered<-order(arrayToAnalyze,decreasing = TRUE)
  #Creating graphic with pie
  p<-pie(arrayToAnalyze[ariOrdered],main=name,
         col=rainbow(length(arrayToAnalyze)),
         labels=mydf$Regioni[ariOrdered],radius =1)
  
}

# createBarPlot_multiple : displays a BarPlot with multiple value
##  input -> arrayToAnalyze : array with multiple indexes to display
createBarPlot_multiple<-function(arrayToAnalyze){
  #Traslate table
  arrayT<-t(arrayToAnalyze)
  #give column name's
  colnames(arrayT)<-namesRegion()
  #divide all for 1000
  arrayT<-arrayT/1000
  
  #Creating BarPlot
  barplot(arrayT, main="Grafico a barre multiple",
          legend=namesGarbage(),col=rainbow(6),las=2)
}

# createHistoì: display histogram
# input -> name: gives a name to the Histogram
#         arrayToAnalyze: array with index to display
createHisto<-function(name,arrayToAnalyze){
  
  #calculating frequencies
  mySum = sum(arrayToAnalyze)
  arrayToAnalyze <- arrayToAnalyze / mySum
  h<-hist(arrayToAnalyze,freq=FALSE,main=name,ylab="Frequenza assoluta dei rifiuti",col=rainbow(length(arrayToAnalyze)))
  str(h)
}

