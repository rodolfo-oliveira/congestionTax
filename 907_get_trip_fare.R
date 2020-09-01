library(dplyr)

#divide o valor das viagens integradas
get_trip_fare <- function(aux){
  aux <- flag_integration(aux)
  
  for(i in 1:length(aux$trecho)){
    if(aux$integrado[i]==0){
      if(aux$modal[i] == 4 & 4 %in% aux$modal[0:(i-1)]){
        aux$valor[i] <- 0
      }
      else{
        aux$valor[i] <- get_fare(aux[i,])
      }
    }
    else if(aux$modal[i] %in% c(1,2,3)){
      if(aux$integrado[i] == 4){
        aux$valor[i] <- get_fare(aux[i,])/2
        aux$valor[i-1] <- get_fare(aux[i,])/2
      }
    }else if(aux$modal[i] == 4){
      if(aux$modal[i] == 4 & 4 %in% aux$modal[0:(i-1)]){
        aux$valor[i] <- 0
      }
      else if(aux$integrado[i] %in% c(1,2,3) & i==1){
        aux$valor[i] <- get_fare(aux[i,])/2
        aux$valor[i-1] <- get_fare(aux[i,])/2
      }
      else if(i>=2){
        if(aux$integrado[i] %in% c(1,2,3) & aux$integrado[i-1] == 0){
          aux$valor[i] <- get_fare(aux[i,])/2
          aux$valor[i-1] <- get_fare(aux[i,])/2
        }
      }
      else if(i>=3){
        if(aux$integrado[i-1] %in% c(1,2,3) & aux$integrado[i-2] == 0){
          aux$valor[i] <- get_fare(aux[i,])/2
          aux$valor[i-2] <- get_fare(aux[i,])/2
        }
      }
      else if(i>=4){
        if(aux$integrado[i-2] %in% c(1,2,3) & aux$integrado[i-3] == 0){
          aux$valor[i] <- get_fare(aux[i,])/2
          aux$valor[i-3] <- get_fare(aux[i,])/2
        }
      }
    }
    
  }
  return(aux)
}
