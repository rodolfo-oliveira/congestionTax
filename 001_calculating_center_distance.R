library(osrm)
options(osrm.server = "http://127.0.0.1:5000/")
library(dplyr)
library(rgdal)
source('501_OD_processing_functions.R')

#abrindo base da OD
database <- foreign::read.dbf("OD 2017/Banco de dados/OD_2017.dbf")

#filtrando para transporte motorizado individual
database <- database[database$MODOPRIN %in% 9:12,]

#espacializando origens e destinos
origens <- convert_spatial(database = database[is.na(database$CO_D_X)==F,])
destinos <- convert_spatial(database = database[is.na(database$CO_D_X)==F,], origem = F)




#abrindo a zona do centro expandido
centro <- readOGR('OD 2017/Mapas/Shape/SIRGAS_SHP_restricaoveiculomian/SIRGAS_SHP_restricaoveiculomian.shp', p4s = proj4string(origens))

ids <- which(is.na(over(origens, centro))[,1]==FALSE | is.na(over(destinos, centro))[,1]==FALSE)

database$km <- 0
database$km_pedagio <- 0





for(i in 1:length(database$ZONA)){
  
  res <- distancia_viagem_centro(origem = origens[i,], 
                                 destino = destinos[i,],
                                 area = centro)
  
  database$km[i] <- res[1]
  database$km_pedagio[i] <- res[2]
  
  print(c(i," ", database$km[i], database$km_pedagio[i]))
  #Sys.sleep(0.9)
}
  
write.csv(database, "Base_alterada.csv")
  