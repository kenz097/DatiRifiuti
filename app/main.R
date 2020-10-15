mydf <- Dati
mydf
for (i in 1:20){
  for (j in 2:7) {
    mydf[i, j] <- as.integer(mydf[i, j])
  }
}
remove(i, j)


main <- function() {
  
  # Getting the row arrays
  arrayRaccoltaIndifferenziata = mydf$`Raccolta indifferenziata`
  arrayCarta = mydf$`Carta e cartone`
  arrayPlastica = mydf$Plastica
  arrayVetro = mydf$Vetro
  arrayUmido = mydf$`Rifiuti organici`
  arrayAltro = mydf$Altro
  
}

main()
