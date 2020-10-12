#fonte http://www.emtu.sp.gov.br/dadosAbertosEmtu/relatorios/dadosoperacionais.htm#

tarifas_EMTU <- function(){
  require(data.table)
  require(rgdal)
  
  EMTU_dados <- fread("dados_passagens/RelacaoDeLinhasEMTU_RMSP_09_10_2020.csv",encoding = "Latin-1")
  
  EMTU_dados$Denominacao_A_proc <- trimws(stringr::str_remove_all(EMTU_dados$Denominacao_A, "\\(.+\\)"))
  EMTU_dados$Denominacao_A_proc <- stringr::str_remove_all(EMTU_dados$Denominacao_A_proc, " CENTRO\\)")
  EMTU_dados$Denominacao_A_proc <- stringr::str_replace_all(EMTU_dados$Denominacao_A_proc, "^SP$", "SAO PAULO")
  EMTU_dados$Denominacao_A_proc <- stringr::str_replace_all(EMTU_dados$Denominacao_A_proc, "^SBC$", "SAO BERNARDO DO CAMPO")
  
  EMTU_dados$Denominacao_B_proc <- trimws(stringr::str_remove_all(EMTU_dados$Denominacao_B, "\\(.+\\)"))
  EMTU_dados$Denominacao_B_proc <- stringr::str_remove_all(EMTU_dados$Denominacao_B_proc, " CENTRO\\)")
  EMTU_dados$Denominacao_B_proc <- stringr::str_replace_all(EMTU_dados$Denominacao_B_proc, "^SP$", "SAO PAULO")
  EMTU_dados$Denominacao_B_proc <- stringr::str_replace_all(EMTU_dados$Denominacao_B_proc, "^SBC$", "SAO BERNARDO DO CAMPO")
  
  OD_dados <- readOGR("OD 2017/Mapas/Shape/Municipios_2017_region.shp")
  OD_dados <- OD_dados@data
  
  aux <- stringr::str_replace_all(toupper(OD_dados$NomeMunici),pattern = "[ÃÁÀ]",replacement = "A")
  aux <- stringr::str_replace_all(toupper(aux),pattern = "[ÍÌ]",replacement = "I")
  aux <- stringr::str_replace_all(toupper(aux),pattern = "[ÓÒ]",replacement = "O")
  aux <- stringr::str_replace_all(toupper(aux),pattern = "[Ç]",replacement = "C")
  aux <- stringr::str_replace_all(toupper(aux),pattern = "[É]",replacement = "E")
  aux <- stringr::str_replace_all(toupper(aux),pattern = "BIRITIBA-MIRIM",replacement = "BIRITIBA MIRIM")
  OD_dados$NomeMunici <- aux
  
  EMTU_dados$tariff <- as.numeric(stringr::str_replace_all(EMTU_dados$Tarifa,pattern = ",", "."))
  
  EMTU_dados %>%
    group_by(Denominacao_A_proc,Denominacao_B_proc) %>%
    summarise(tarifa_media = mean(tariff)) -> OD_EMTU
  
  names(OD_EMTU) <- c("origem", "destino", "tarifa")
  
  EMTU_dados %>%
    group_by(Denominacao_B_proc,Denominacao_A_proc) %>%
    summarise(tarifa_media = mean(tariff)) -> aux
  
  names(aux) <- c("origem", "destino", "tarifa")
  
  OD_EMTU <- rbind(OD_EMTU, aux)
  OD_EMTU <- unique(OD_EMTU)
  
  for(i in 1:length(OD_dados$NumeroMuni)){
    #print(paste(OD_EMTU$origem[OD_EMTU$origem == OD_dados$NomeMunici[i]], ", ", OD_dados$NumeroMuni[OD_dados$NomeMunici == OD_dados$NomeMunici[i]]))
    #print(paste(OD_EMTU$destino[OD_EMTU$destino == OD_dados$NomeMunici[i]], ", ", OD_dados$NumeroMuni[OD_dados$NomeMunici == OD_dados$NomeMunici[i]]))
    OD_EMTU$origem[OD_EMTU$origem == OD_dados$NomeMunici[i]] <- as.numeric(as.character(OD_dados$NumeroMuni[OD_dados$NomeMunici == OD_dados$NomeMunici[i]]))
    OD_EMTU$destino[OD_EMTU$destino == OD_dados$NomeMunici[i]] <- as.numeric(as.character(OD_dados$NumeroMuni[OD_dados$NomeMunici == OD_dados$NomeMunici[i]]))
  }
  
  return(OD_EMTU)
}







         