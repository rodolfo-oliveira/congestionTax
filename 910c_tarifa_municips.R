#fonte - https://plamurbblog.wordpress.com/2020/01/28/missao-impossivel-tarifas-de-onibus-municipais-das-39-cidades-da-rmsp/

tarifas_municips <- function(){
  require(rgdal)
  
  auxiliar1 <- fread("dados_passagens/RelacaoDeLinhasEMTU_RMSP_09_10_2020.csv",encoding = "Latin-1")
  auxiliar1$Denominacao_A_proc <- trimws(stringr::str_remove_all(auxiliar1$Denominacao_A, "\\(.+\\)"))
  auxiliar1$Denominacao_A_proc <-  stringr::str_remove_all(auxiliar1$Denominacao_A_proc, " CENTRO\\)")
  auxiliar1$Denominacao_A_proc <- stringr::str_replace_all(auxiliar1$Denominacao_A_proc, "^SP$", "SAO PAULO")
  auxiliar1$Denominacao_A_proc <- stringr::str_replace_all(auxiliar1$Denominacao_A_proc, "^SBC$", "SAO BERNARDO DO CAMPO")
  
  varnames <- c(unique(auxiliar1$Denominacao_A_proc)[order(unique(auxiliar1$Denominacao_A_proc))])
  aux <- data.frame(matrix(ncol = length(varnames), nrow = 1))
  names(aux) <- varnames
  
  aux$ARUJA <- 4.5
  aux$BARUERI <- 4.5
  aux$`BIRITIBA MIRIM` <- 4.5
  aux$CAIEIRAS <- 4.8
  aux$CAJAMAR <- 4.6
  aux$CARAPICUIBA <- 4.5
  aux$COTIA <- 4.5
  aux$DIADEMA <- 4.65
  aux$`EMBU DAS ARTES` <- 4
  aux$`EMBU-GUACU` <- 3.5
  aux$`FERRAZ DE VASCONCELOS` <- 4.4
  aux$`FRANCISCO MORATO` <- 4.6
  aux$`FRANCO DA ROCHA` <- 4.8
  aux$GUARAREMA <- 4.5
  aux$GUARULHOS <- 4.45
  aux$`ITAPECERICA DA SERRA` <- 3.9
  aux$ITAPEVI <- 4.5
  aux$ITAQUAQUECETUBA <- 4.4
  aux$JANDIRA <- 4.4
  aux$JUQUITIBA <- 3.5
  aux$MAIRIPORA <- 4.7
  aux$MAUA <- 4.3
  aux$`MOGI DAS CRUZES` <- 4.5
  aux$OSASCO <- 4.5
  aux$`PIRAPORA DO BOM JESUS` <- 0
  aux$POA <- 4.4
  aux$`RIBEIRAO PIRES` <- 4.4
  aux$`RIO GRANDE DA SERRA` <- 4.2
  aux$SALESOPOLIS <- NA
  aux$`SANTA ISABEL` <- 4.35
  aux$`SANTANA DE PARNAIBA` <- 4.5
  aux$`SANTO ANDRE` <- 4.75
  aux$`SAO BERNARDO DO CAMPO` <- 4.75
  aux$`SAO CAETANO DO SUL` <- 4.5
  aux$`SAO LOURENCO DA SERRA` <- NA
  aux$`SAO PAULO` <- 4.4
  aux$SUZANO <- 4.4
  aux$`TABOAO DA SERRA` <- 3.8
  aux$`VARGEM GRANDE PAULISTA` <- 0
    
  aux <- data.frame(mun = names(aux), tarifa = as.numeric(aux[1,]))
  
  aux$mun <- as.character(aux$mun)
  
  OD_dados <- readOGR("OD 2017/Mapas/Shape/Municipios_2017_region.shp")
  OD_dados <- OD_dados@data
  
  auxiliar2 <- stringr::str_replace_all(toupper(OD_dados$NomeMunici),pattern = "[ÃÁÀ]",replacement = "A")
  auxiliar2 <- stringr::str_replace_all(toupper(auxiliar2),pattern = "[ÍÌ]",replacement = "I")
  auxiliar2 <- stringr::str_replace_all(toupper(auxiliar2),pattern = "[ÓÒ]",replacement = "O")
  auxiliar2 <- stringr::str_replace_all(toupper(auxiliar2),pattern = "[Ç]",replacement = "C")
  auxiliar2 <- stringr::str_replace_all(toupper(auxiliar2),pattern = "[É]",replacement = "E")
  auxiliar2 <- stringr::str_replace_all(toupper(auxiliar2),pattern = "BIRITIBA-MIRIM",replacement = "BIRITIBA MIRIM")
  OD_dados$NomeMunici <- auxiliar2
  
  for(i in 1:length(OD_dados$NumeroMuni)){
    #print(paste(OD_EMTU$origem[OD_EMTU$origem == OD_dados$NomeMunici[i]], ", ", OD_dados$NumeroMuni[OD_dados$NomeMunici == OD_dados$NomeMunici[i]]))
    #print(paste(OD_EMTU$destino[OD_EMTU$destino == OD_dados$NomeMunici[i]], ", ", OD_dados$NumeroMuni[OD_dados$NomeMunici == OD_dados$NomeMunici[i]]))
   aux$mun[aux$mun == OD_dados$NomeMunici[i]] <- as.numeric(as.character(OD_dados$NumeroMuni[OD_dados$NomeMunici == OD_dados$NomeMunici[i]]))
  }
  
  return(aux)  
}