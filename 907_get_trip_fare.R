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
        if(aux$modal[i]==4){
          aux$valor[which(aux$integracao_BU_onibus_metro == 1)[1]] <- sp_fares$tarifa_integracao_metro
          aux$valor[i] <- sp_fares$tarifa_integracao_onibus
        }else if (aux$modal[i] %in% c(1,2,3)){
          aux$valor[which(aux$integracao_BU_onibus_metro == 1)[1]] <- sp_fares$tarifa_integracao_onibus
          aux$valor[i] <- sp_fares$tarifa_integracao_metro
        }
      }
    }
    
  }
  return(aux)
}
