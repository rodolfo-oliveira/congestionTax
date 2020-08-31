#funcao para captar integracoes
#TODO integrações além da CPTM, Metro e SPTrans
library(dplyr)

flag_integration <- function(aux){
  onibus <- 0
  metro <- 0
  cptm <- 0
  
  for(i in 1:length(aux$trecho)){
    if(i==1){
      if(aux$modal[i] == 1) metro = 1
      if(aux$modal[i] == 2) cptm = 1
      if(aux$modal[i] == 3) metro = 1
      if(aux$modal[i] == 4){
          onibus = 1
          aux$SPTRANS[i] <- T
      }
      mod_ant <- aux$modal[i]
    }
    
    if(i>1){
      if(aux$modal[i] == 1){
        if(onibus == 1){
          aux$integrado[i] <- mod_ant
          metro <- 1
        }
        else if(metro == 1){
          aux$integrado[i] <- 0
        }
        else if(cptm == 1){
          aux$integrado[i] <- mod_ant
          metro <- 1
          cptm <- 0
        }
        else{
          aux$integrado[i] <- 0
          metro <- 1
        }
      }
      
      if(aux$modal[i] == 2){
        if(onibus == 1){
          aux$integrado[i] <- mod_ant
          cptm <- 1
        }
        else if(metro == 1){
          aux$integrado[i] <- mod_ant
          metro <- 0
          cptm <- 1

        }
        else if(cptm == 1){
          aux$integrado[i] <- 0

        }
        else{
          aux$integrado[i] <- 0
          cptm <- 1
        }
      }
      if(aux$modal[i] == 3){
        if(onibus == 1){
          aux$integrado[i] <- mod_ant
          cptm <- 1

        }
        else if(metro == 1){
          aux$integrado[i] <- mod_ant
          metro <- 0
          cptm <- 1

        }
        else if(cptm == 1){
          aux$integrado[i] <- 0

        }
        else{
          aux$integrado[i] <- 0
          cptm <- 1
          }
        }
      
      if(aux$modal[i] == 4){
        if(onibus == 1){
          aux$integrado[i] <- mod_ant

        }
        else if(metro == 1){
          aux$integrado[i] <- mod_ant
          metro <- 0
          onibus <- 1
          }
        else if(cptm == 1){
          aux$integrado[i] <- mod_ant
          cptm <- 0
          onibus <- 1
        }
        else{
          aux$integrado[i] <- 0
          onibus <- 1
        }
      }
      mod_ant <- aux$modal[i]       
    }
  }
  return(aux)
}
