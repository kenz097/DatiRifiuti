# createPareto : displays a Pareto diagram with frequences
##  input -> name : gives a name to the pareto
##           arrayToAnalyze : array with indexes to display
createPareto <- function (name, arrayToAnalyze) {
  
  # Calculating frequences
  mySum = sum(arrayToAnalyze)
  arrayToAnalyze <- arrayToAnalyze / mySum
  
  # Ordering 
  ari_ordered <- order(arrayToAnalyze, decreasing = TRUE)
  
  # Creating graphic
  bp <- barplot(arrayToAnalyze[ari_ordered], main = name,
                col=rainbow(length(arrayToAnalyze)), 
                names = mydf$Regioni[ari_ordered], las=2, ylim = c(0, 1.05))
  
  lines(bp, cumsum(arrayToAnalyze[ari_ordered]), 
        type = "b", pch = 16)
  
  text(bp - 0.2, cumsum(arrayToAnalyze[ari_ordered]) + 0.03,
       paste(format(cumsum(arrayToAnalyze[ari_ordered]) * 100,
                    digits = 2), "%"))
}

# createBarPlot_base : displays a BarPlot diagram
##  input -> name : gives a name to the pareto
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
##  input -> name : gives a name to the pareto
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
##  input -> name : gives a name to the pareto
##           arrayToAnalyze : array with indexes to display
##           maxLimit : the y limit on the axis
##           frequency : 0 -> use indexes | 1 -> use frequences
createBarPlot_ordered <- function (name, arrayToAnalyze, maxLimit, frequency) {
  if (frequency == 1) {
    mySum = sum(arrayToAnalyze)
    arrayToAnalyze <- arrayToAnalyze / mySum
  }
  data_order <- order(arrayToAnalyze, decreasing = TRUE)
  barplot(arrayToAnalyze[data_order],
          col=rainbow(length(arrayToAnalyze)),
          ylim = c(0, maxLimit), main = name,
          names = mydf$Regioni[data_order], las=2)
}

# createBoxPlot_base : displays a BoxPlot with default settings
##  input -> name : gives a name to the pareto
##           arrayToAnalyze : array with indexes to display
createBoxPlot_base <- function(name, arrayToAnalyze) {
  boxplot(arrayToAnalyze, col="green", main = name)
}

# createBoxPlot_colored : displays a BoxPlot with custom color
##  input -> name : gives a name to the pareto
##           arrayToAnalyze : array with indexes to display
##           customColor : color of the BoxPlot
createBoxPlot_colored <- function(name, arrayToAnalyze, customColor) {
  boxplot(arrayToAnalyze, col=customColor, main = name)
}
#This function will create Pie diagram based on the array parameter and name
#of graphic
createPie<-function(arrayToAnalyze,nameGf){
  # Ordering
  ariOrdered<-order(arrayToAnalyze,decreasing = TRUE)
  #Creating graphic with pie
  p<-pie(arrayToAnalyze[ariOrdered],main=nameGf,
         col=rainbow(length(arrayToAnalyze)),
         labels=mydf$Regioni[ariOrdered],radius =1)
  
}
