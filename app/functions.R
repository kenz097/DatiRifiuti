# This function will create a Pareto diagram based on the array parameter
createPareto <- function (array_to_analyze){
  # Sum all items
  mySum = sum(array_to_analyze)
  array_to_analyze <- array_to_analyze / mySum
  
  # Ordering 
  ari_ordered <- order(array_to_analyze, decreasing = TRUE)
  
  # Creating graphic
  bp <- barplot(array_to_analyze[ari_ordered],
                col=rainbow(length(array_to_analyze)), 
                names = mydf$Regioni[ari_ordered], las=2, ylim = c(0, 1.05))
  
  lines(bp, cumsum(array_to_analyze[ari_ordered]), 
        type = "b", pch = 16)
  
  text(bp - 0.2, cumsum(array_to_analyze[ari_ordered]) + 0.03,
       paste(format(cumsum(array_to_analyze[ari_ordered]) * 100,
                    digits = 2), "%"))
}