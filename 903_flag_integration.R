#funcao para captar integracoes
#TODO integrações além da CPTM, Metro e SPTrans
library(dplyr)

flag_integration <- function(aux){
  
  for(i in 1:length(aux$trecho)){
    if(aux$modal[i] == 4){
      
      if(aux$integracao_BU_onibus_metro[i] == 0){
        if(2 %in% aux$integracao_BU_onibus_metro[1:i] == F & 3 %in% aux$integracao_BU_onibus_metro[1:i] == F) aux$integracao_BU_onibus_metro[i:length(aux$trecho)] <- 1
      }else{
        if(TRUE %in% (aux$integracao_BU_metro!=0)){
          aux$integracao_BU_onibus_metro[i:length(aux$trecho)] <- 0
          if(2 %in% aux$integracao_BU_onibus_metro[1:i] == F & 3 %in% aux$integracao_BU_onibus_metro[1:i] == F) aux$integracao_BU_onibus_metro[i] <- 3
        }
      }
      if(aux$integracao_BU_onibus[i] == 0){
        aux$integracao_BU_onibus[i:length(aux$trecho)] <- 1
        aux$integracao_BU_onibus[i] <- 2
      }
      
      aux$integracao_BU_metro[i:length(aux$trecho)] <- 0
      
    }else if(aux$modal[i] %in% c(1,2,3)){
      
      if(aux$integracao_BU_onibus_metro[i] == 0){
        if(2 %in% aux$integracao_BU_onibus_metro[1:i] == F & 3 %in% aux$integracao_BU_onibus_metro[1:i] == F) aux$integracao_BU_onibus_metro[i:length(aux$trecho)] <- 1
      }else if (aux$integracao_BU_metro[i] == 0){
        aux$integracao_BU_onibus_metro[i:length(aux$trecho)] <- 0
        if(2 %in% aux$integracao_BU_onibus_metro[1:i] == F & 3 %in% aux$integracao_BU_onibus_metro[1:i] == F) aux$integracao_BU_onibus_metro[i] <- 3
      }
      if(aux$integracao_BU_metro[i] == 0){
       aux$integracao_BU_metro[i:length(aux$trecho)] <- 1
       aux$integracao_BU_metro[i] <- 2 
      }
    }
    else {
      aux$integracao_BU_metro[i:length(aux$trecho)] <- 0
    }
  }
  
  
  aux$SPTRANS[which(aux$modal==4)] <- TRUE
  return(aux)
}
