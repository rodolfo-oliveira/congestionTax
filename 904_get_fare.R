#funcao para retornar o valor da tarifa
library(dplyr)

get_fare <- function(aux,i){
  if(aux$pag_viag %in% 2) VT <- T
  else VT <- F
  
  if(aux$modal %in% c(1,2,3)){
    
    if(aux$integracao_BU_metro == 1){
      return(0)
    }
    
    else if(aux$integracao_BU_onibus_metro == 3){
      if(VT == T){
        return(8.85)
      }
      else{
        return(7.65)
      }
    }
    
    else if(aux$integracao_BU_metro==2){
      return(4.40)
    }
  }
  
  if(aux$modal == 4){
    
    if(aux$integracao_BU_onibus_metro == 3){
      if(VT == T){
        return(8.85)
      }
      else{
        return(7.65)
      }
    }
    
    else if(aux$integracao_BU_onibus == 1){
      return(0)
    }
    
    if(aux$integracao_BU_onibus == 2){
      if(VT == T){
        return(4.83)
      }
      else{
        return(4.40)
      }
    }
  }
  return(NA)
}
