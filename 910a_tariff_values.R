#tariff values

tariff_values <- function(){
  
  varnames <- c("tarifa_SPtrans_base",
               "tarifa_SPtrans_Metro_integracao",
               "tarifa_SPtrans_VT",
               "tarifa_Metro_VT",
               "tarifa_SPTrans_Metro_VT_integracao",
               "tarifa_SPTrans_Metro_VT_integracao_madrugador",
               "tarifa_SPtrans_Metro_integracao_madrugador",
               "tarifa_SPtrans_estudante")
  
  
        aux <- data.frame(matrix(ncol = length(varnames), nrow = 1))
        names(aux) <- varnames
  
        aux$tarifa_SPtrans_base <- 4.4
        aux$tarifa_SPtrans_Metro_integracao <- 7.65
        aux$tarifa_SPtrans_VT <- 4.83
        aux$tarifa_Metro_VT <- 4.4
        aux$tarifa_SPTrans_Metro_VT_integracao <- 8.85
        aux$tarifa_SPTrans_Metro_VT_integracao_madrugador <- 8.06
        aux$tarifa_SPtrans_Metro_integracao_madrugador <- 6.86
        aux$tarifa_SPtrans_estudante <- 2.2

        return(aux)
        }