library(dplyr)

source('501_OD_processing_functions.R')

#abrindo base da OD
database <- foreign::read.dbf("OD 2017/Banco de dados/OD_2017.dbf")

#filtrando para viagens feitas
database <- database[is.na(database$N_VIAG) == F,]

#calculando os preços de viagens
database <- estimate_fare(database)

write.csv2(database, "base_arrecadacao_SPTrans.csv")
      
  