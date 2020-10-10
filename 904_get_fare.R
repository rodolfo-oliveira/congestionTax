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
          return(sp_fares$tarifa_SPTrans_Metro_VT_integracao_madrugador)
        }
        return(sp_fares$tarifa_SPTrans_Metro_VT_integracao)
      }
      else{
        if(aux$estudante[i] == T) return(sp_fares$tarifa_SPtrans_estudante)
        
        else if(madrugador == T){
          return(sp_fares$tarifa_SPtrans_Metro_integracao_madrugador)
        }else{
          return(sp_fares$tarifa_SPtrans_Metro_integracao)
        }
      }
    }
    
    else if(aux$integracao_BU_metro[i]==2){
      if(aux$estudante[i]) return(sp_fares$tarifa_SPtrans_estudante)
      return(sp_fares$tarifa_SPtrans_base)
    }
  }
  
  if(aux$modal[i] == 4){
    
    if(isento == T) return(0)
    
    if(aux$integracao_BU_onibus_metro[i] == 3){
      if(aux$estudante[i] == T) return(sp_fares$tarifa_SPtrans_estudante)
      if(VT == T){
        return(sp_fares$tarifa_SPTrans_Metro_VT_integracao)
      }
      else{
        return(sp_fares$tarifa_SPtrans_Metro_integracao)
      }
    }
    
    else if(aux$integracao_BU_onibus[i] == 1){
      return(0)
    }
    
    if(aux$integracao_BU_onibus[i] == 2){
      if(VT == T){
        return(sp_fares$tarifa_SPtrans_VT)
      }
      else{
        if(aux$estudante[i] == T) return(sp_fares$tarifa_SPtrans_estudante)
        else return(sp_fares$tarifa_SPtrans_base)
      }
    }
  }
  if(aux$modal[i] == 5){
    return(tarif_muni$tarifa[tarif_muni$mun==aux$mun_origin[i]])
  }
  if(aux$modal[i] == 6){
    return(EMTU_dados$tarifa[EMTU_dados$origem==aux$mun_origin & EMTU_dados$destino == aux$mun_dest])
  }
  return(NA)
}
