#function to adjust tariff values


correcting_tariffs <- function(database_line){
  #1 ajuste -> apagar os pagamentos no período de integração do bilhete unico para ônibus
  
  indicador_BU <- 0
  indicador_bu_hora <- strptime(paste0("00", ":", "01"), format = "%H:%M")
  for(i in 1:length(database_line$ZONA)){
    if(4 %in% database_line[i,c("MODO1","MODO2","MODO3","MODO4")]){
      if(indicador_BU == 1){
        if(strptime(paste0(database_line$H_SAIDA[i], ":", database_line$MIN_SAIDA[i]), format = "%H:%M") < indicador_bu_hora + hours(3)){
          database_line$valor_total[i] <- database_line$valor_total[i] - database_line$valor_SPTtrans[i]
          database_line$valor_SPTtrans[i] <- 0
        }else{
          indicador_bu_hora <- strptime(paste0(database_line$H_SAIDA[i], ":", database_line$MIN_SAIDA[i]), format = "%H:%M")
        }
      }else{
        indicador_BU <- 1
        indicador_bu_hora <- strptime(paste0(database_line$H_SAIDA[i], ":", database_line$MIN_SAIDA[i]), format = "%H:%M")
      }
    }
    
  }
  return(database_line)
}