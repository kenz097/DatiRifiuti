mydf <- Dati
mydf
for (i in 1:28){
  for (j in 2:7) {
    mydf[i, j] <- as.integer(mydf[i, j])
  }
}


main <- function() {
  
  # Getting the row array
  arrayRaccoltaIndifferenziata = mydf$`Raccolta indifferenziata`
  createPareto(arrayRaccoltaIndifferenziata)
  
}

main()
