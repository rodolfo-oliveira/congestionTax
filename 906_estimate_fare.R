#funcao para estimar o valor das corridas
library(dplyr)

estimate_fare <- function(database){
  
  fares <- estimate_fare_unit(database[1,])
  
  for(i in 2:length(database$ZONA)){
    aux <- estimate_fare_unit(database_line = database[i,])
    fares <- rbind(fares, aux)
    print(paste0("Andamento do cálculo das tarifas: ", round(100*i/length(database$ZONA),digits = 2), "%"))
  }
  
  return(cbind(database, fares))

}
