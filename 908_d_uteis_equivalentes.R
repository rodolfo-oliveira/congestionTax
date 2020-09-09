
#cálculo de dias úteis equivalentes
#demanda mensal

d_uteis_equivalentes <- function(start = as.Date("2017-01-01", origin = "1980-01-01"), end = as.Date("2017-12-31", origin = "1980-01-01")) {
  
  require(lubridate)
    
    start = as.Date(start, origin = "1980-01-01")
    end = as.Date(end, origin = "1980-01-01")
    
  dias_equivalentes_totais <- 0
  
  start <- as.Date("2017-01-01", origin = "1980-01-01")
  end <- as.Date("2017-12-31", origin = "1980-01-01")
  
  for(i in month(start):month(end)){
    dias_equivalentes <- 0
    aux1 <- start + months(i-1) 
    aux2 <- start + months(i) - days(1)
    dias <- weekdays.Date(seq(from = aux1,to = aux2,by = 1))
    
    dias <- summary(as.factor(dias))
    
    dias_equivalentes <- dias_equivalentes + as.numeric(dias["domingo"]*1/3)
    dias_equivalentes <- dias_equivalentes + as.numeric(dias["sábado"]*2/3)
    dias_equivalentes <- dias_equivalentes + as.numeric(sum(dias[c("segunda","terça","quarta","quinta","sexta")]))
    
    if(i %in% c(1,7)) dias_equivalentes <- 0.8*dias_equivalentes
    dias_equivalentes_totais <- dias_equivalentes_totais + dias_equivalentes
  }
}
