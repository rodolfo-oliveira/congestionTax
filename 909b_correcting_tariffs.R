
vectorized_correcting_tariffs <- function(database){
  source("909a_correcting_tariffs.R")
  
  aux <- unique(database$ID_PESS)
  for(i in 1:length(aux)){
    
    database[database$ID_PESS == aux[i],] <- correcting_tariffs(database_line = database[database$ID_PESS == aux[i],])
    print(paste0("Corrigindo tarifas: ", round(100*i/length(aux),digits = 2),"%"))
  }
  return(database)
}