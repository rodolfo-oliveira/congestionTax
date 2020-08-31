#funcao para estimar o valor de uma corrida
library(dplyr)

estimate_fare_unit <- function(database_line){
  
  #01 - Metrô
  #02 - Trem
  #03 - Monotrilho
  #04 - Ônibus/micro-ônibus/perua do município de São Paulo
  #05 - Ônibus/micro-ônibus/perua de outros municípios
  #06 - Ônibus/micro-ônibus/perua metropolitano
  #07 - Transporte Fretado
  #08 - Transporte Escolar
  #09 - Dirigindo Automóvel
  #10 - Passageiro de Automóvel
  #11 - Táxi Convencional
  #12 - Táxi não Convencional
  #13 - Dirigindo Moto
  #14 - Passageiro de Moto
  #15 - Bicicleta
  #16 - A Pé
  #17 - Outros
 
  trechos <- database_line[c("MODO1","MODO2","MODO3","MODO4")][is.na(database_line[c("MODO1","MODO2","MODO3","MODO4")])==F]
  trechos_ori <- database_line[,c("MUNI_O","MUNI_T1","MUNI_T2","MUNI_T3")][is.na(database_line[c("MODO1","MODO2","MODO3","MODO4")])==F]
  
  aux <- tibble(trecho=0, modal = 0, integrado=0, valor=0, SPTRANS=F, mun_origin = 0, pag_viag = 0)
  aux <- aux[0,]
  
  for(i in 1:length(trechos)){
  
    aux <- rbind(aux, tibble(trecho=i, modal = trechos[i], integrado=0, valor=0, SPTRANS=F, mun_origin = trechos_ori[i], pag_viag = database_line$PAG_VIAG))  
  }
  

   
}