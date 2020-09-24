library(dplyr)

#divide o valor das viagens integradas
get_trip_fare <- function(aux){
  aux <- flag_integration(aux)
  
  for(i in 1:length(aux$trecho)){
    
    if(aux$integracao_BU_onibus_metro[i] != 3){
      aux$valor[i] <- get_fare(aux[i,])
      }else{
      if(aux$estudante[i] == T & 2 %in% aux$pag_viag == F) aux$valor[i] <- get_fare(aux[i,])
      else{
        aux$valor[which(aux$integracao_BU_onibus_metro == 1)[1]] <- get_fare(aux[i,])/2
        aux$valor[i] <- get_fare(aux[i,])/2
      }
    }
    
  }
  return(aux)
}
