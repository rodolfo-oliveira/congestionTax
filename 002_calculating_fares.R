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


#calculando os preços de viagens----
database <- estimate_fare(database)

database2 <- vectorized_correcting_tariffs(database)
database2 <- ind_share_family_income(database2)


base_viagem <- database2[,c("ID_ORDEM","VT","CD_ATIVI", "PAG_VIAG","valor_total","valor_SPTtrans","ren_fam_ind","FE_VIA","ID_PESS","FE_PESS","ID_FAM","FE_FAM")]
write.csv2(base_viagem, "resultados/base_viagens_total_4_40.csv")
base_viagem %>%
  group_by(ID_PESS) %>%
  summarise(ID_PESS = first(ID_PESS),
            FE_PESS = first(FE_PESS),
            CD_ATIVI = first(CD_ATIVI),
            valor_total = sum(valor_total),
            valor_SPTtrans = sum(valor_SPTtrans),
            ren_fam_ind =first(ren_fam_ind),
            ID_FAM = first(ID_FAM),
            FE_FAM = first(FE_FAM)) -> base_pessoa

write.csv2(base_pessoa, "resultados/base_pessoas_4_40.csv")

base_pessoa %>%
  group_by(ID_FAM) %>%
  summarise(ID_FAM = first(ID_FAM),
            FE_FAM = first(FE_FAM),
            valor_total = sum(valor_total),
            valor_SPTtrans = sum(valor_SPTtrans),
            ren_fam =sum(ren_fam_ind)) -> base_familia

write.csv2(base_familia, "resultados/base_familias_4_40.csv")

#rodando para passagem igual a 7,16 para a SPTrans----

sp_fares <<- tariff_values(base_SPTrans = 7.16)
#calculando os preços de viagens
database <- estimate_fare(database)

database2 <- vectorized_correcting_tariffs(database)
database2 <- ind_share_family_income(database2)


base_viagem <- database2[,c("ID_ORDEM","VT","CD_ATIVI", "PAG_VIAG","valor_total","valor_SPTtrans","ren_fam_ind","FE_VIA","ID_PESS","FE_PESS","ID_FAM","FE_FAM")]
write.csv2(base_viagem, "resultados/base_viagens_total_7_16_SPTrans.csv")
base_viagem %>%
  group_by(ID_PESS) %>%
  summarise(ID_PESS = first(ID_PESS),
            FE_PESS = first(FE_PESS),
            CD_ATIVI = first(CD_ATIVI),
            valor_total = sum(valor_total),
            valor_SPTtrans = sum(valor_SPTtrans),
            ren_fam_ind =first(ren_fam_ind),
            ID_FAM = first(ID_FAM),
            FE_FAM = first(FE_FAM)) -> base_pessoa

write.csv2(base_pessoa, "resultados/base_pessoas_7_16_SPTrans.csv")

base_pessoa %>%
  group_by(ID_FAM) %>%
  summarise(ID_FAM = first(ID_FAM),
            FE_FAM = first(FE_FAM),
            valor_total = sum(valor_total),
            valor_SPTtrans = sum(valor_SPTtrans),
            ren_fam =sum(ren_fam_ind)) -> base_familia

write.csv2(base_familia, "resultados/base_familias_7_16_SPTrans.csv")

#rodando para passagem igual a 7,16 para todos os meios
sp_fares <<- tariff_values(base_SPTrans = 7.16,base_metro = 7.16)
#calculando os preços de viagens
database <- estimate_fare(database)

database2 <- vectorized_correcting_tariffs(database)
database2 <- ind_share_family_income(database2)


base_viagem <- database2[,c("ID_ORDEM","VT","CD_ATIVI", "PAG_VIAG","valor_total","valor_SPTtrans","ren_fam_ind","FE_VIA","ID_PESS","FE_PESS","ID_FAM","FE_FAM")]
write.csv2(base_viagem, "resultados/base_viagens_total_7_16.csv")
base_viagem %>%
  group_by(ID_PESS) %>%
  summarise(ID_PESS = first(ID_PESS),
            FE_PESS = first(FE_PESS),
            CD_ATIVI = first(CD_ATIVI),
            valor_total = sum(valor_total),
            valor_SPTtrans = sum(valor_SPTtrans),
            ren_fam_ind =first(ren_fam_ind),
            ID_FAM = first(ID_FAM),
            FE_FAM = first(FE_FAM)) -> base_pessoa

write.csv2(base_pessoa, "resultados/base_pessoas_7_16.csv")

base_pessoa %>%
  group_by(ID_FAM) %>%
  summarise(ID_FAM = first(ID_FAM),
            FE_FAM = first(FE_FAM),
            valor_total = sum(valor_total),
            valor_SPTtrans = sum(valor_SPTtrans),
            ren_fam =sum(ren_fam_ind)) -> base_familia

write.csv2(base_familia, "resultados/base_familias_7_16.csv")



