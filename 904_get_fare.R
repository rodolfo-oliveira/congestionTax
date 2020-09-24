#funcao para retornar o valor da tarifa
library(dplyr)

get_fare <- function(aux,i){
  if(2 %in% aux$pag_viag) VT <- T
  else VT <- F
  
  if(3 %in% aux$pag_viag) isento <- T
  else isento <- F
  
  if(TRUE %in% aux$madrugador) madrugador <- T
  else madrugador <- F
  
  if(aux$modal[i] %in% c(1,2,3)){
    
    if(isento == T) return(0)
    

    
    if(aux$integracao_BU_metro[i] == 1){
      return(0)
    }
    
    else if(aux$integracao_BU_onibus_metro[i] == 3){
      if(VT == T){
        if(madrugador == T){
          return(8.06)
        }
        return(8.85)
      }
      else{
        if(aux$estudante[i] == T) return(2.20)
        
        else if(madrugador == T){
          return(6.86)
        }else{
          return(7.65)
        }
      }
    }
    
    else if(aux$integracao_BU_metro[i]==2){
      if(aux$estudante[i]) return(2.20)
      return(4.40)
    }
  }
  
  if(aux$modal[i] == 4){
    
    if(isento == T) return(0)
    
    if(aux$integracao_BU_onibus_metro[i] == 3){
      if(aux$estudante[i] == T) return(2.20)
      if(VT == T){
        return(8.85)
      }
      else{
        return(7.65)
      }
    }
    
    else if(aux$integracao_BU_onibus[i] == 1){
      return(0)
    }
    
    if(aux$integracao_BU_onibus[i] == 2){
      if(VT == T){
        return(4.83)
      }
      else{
        if(aux$estudante[i] == T) return(2.20)
        else return(4.40)
      }
    }
  }
  return(NA)
}
