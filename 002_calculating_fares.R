library(dplyr)

source('501_OD_processing_functions.R')

#abrindo base da OD
database <- foreign::read.dbf("OD 2017/Banco de dados/OD_2017.dbf")

#filtrando para viagens feitas
database <- database[is.na(database$N_VIAG) == F,]
#filtrando para viagens que contam com trechos de transporte publico
database <- database[database$MODO1 %in% c(1,2,3,4,5,6) |
                       database$MODO2 %in% c(1,2,3,4,5,6) |
                       database$MODO3 %in% c(1,2,3,4,5,6) |
                       database$MODO4 %in% c(1,2,3,4,5,6),]


#calculando os preços de viagens
database <- estimate_fare(database)

database2 <- vectorized_correcting_tariffs(database)





teste <- database2[,c("MODO1","MODO2","MODO3","MODO4","valor_total","valor_SPTtrans","VT","PAG_VIAG","FE_VIA","FE_PESS","H_SAIDA","CD_ATIVI","N_VIAG","ID_PESS")]


sum(teste$valor_SPTtrans*teste$FE_VIA)*300

sum(teste$valor_SPTtrans*teste$FE_VIA)*300

#pessoa
database2 %>%
  group_by(ID_PESS) %>%
  summarise(total = sum(valor_total), total_sptrans = sum(valor_SPTtrans)) -> pessoa

merge(pessoa, unique(database2[,c("ID_PESS","FE_PESS")]), by="ID_PESS") -> pessoa

sum(pessoa$total_sptrans*pessoa$FE_PESS)*300


database2 %>%
  group_by(ID_DOM) %>%
  summarise(total = sum(valor_total), total_sptrans = sum(valor_SPTtrans)) -> domicilio

merge(domicilio, unique(database2[,c("ID_DOM","FE_DOM")]), by="ID_DOM") -> domicilio

sum(domicilio$total_sptrans*domicilio$FE_DOM)*300

database2 %>%
  group_by(ID_FAM) %>%
  summarise(total = sum(valor_total), total_sptrans = sum(valor_SPTtrans)) -> familia

merge(familia, unique(database2[,c("ID_FAM","FE_FAM")]), by="ID_FAM") -> familia

sum(familia$total_sptrans*familia$FE_FAM)*300


write.csv2(database, "base_arrecadacao_SPTrans.csv")
      
  