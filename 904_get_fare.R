#funcao para retornar o valor da tarifa
library(dplyr)

get_fare <- function(aux){
  if(aux$pag_viag %in% 2) VT <- T
  else VT <- F
  
  if(aux$modal %in% c(1,2,3)){
    
    if(aux$integrado %in% c(1,2,3)){
      return(0)
    }
    
    if(aux$integrado == 4){
      if(VT == T){
        return(8.85)
      }
      else{
        return(7.65)
      }
    }
    
    if(aux$integrado == 0){
      return(4.40)
    }
  }
  
  if(aux$modal == 4){
    
    if(aux$integrado %in% c(1,2,3)){
      if(VT == T){
        return(8.85)
      }
      else{
        return(7.65)
      }
    }
    
    if(aux$integrado == 4){
      return(0)
    }
    
    if(aux$integrado == 0){
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
