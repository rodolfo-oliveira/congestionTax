#tariff values

tariff_values <- function(base_SPTrans = 4.4, base_metro = 4.4){
  
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
  
        #considerando base sp 4.4  
        desconto_metro <- 4.02/4.4
        desconto_sptrans <- 3.63/4.4
        vt_value <- 1 + (4.83 - 4.4)/4.40
        madrugador_discount <- 1 - (8.85 - 8.06)/(4.4*desconto_metro)
        
        
        
        aux$tarifa_SPtrans_base <- base_SPTrans
        aux$tarifa_SPtrans_Metro_integracao <- (desconto_sptrans * base_SPTrans + desconto_metro*base_metro) 
        aux$tarifa_integracao_metro <- base_metro * desconto_metro
        aux$tarifa_integracao_onibus <- base_SPTrans * desconto_sptrans
        aux$tarifa_SPtrans_VT <- base_SPTrans * vt_value
        aux$tarifa_Metro_VT <- base_metro
        aux$tarifa_SPTrans_Metro_VT_integracao <- (base_metro)*desconto_metro + base_SPTrans*vt_value
        aux$tarifa_SPTrans_Metro_VT_integracao_madrugador <- base_metro * madrugador_discount*desconto_metro + base_SPTrans*vt_value
        aux$tarifa_SPtrans_Metro_integracao_madrugador <- base_metro * madrugador_discount * desconto_metro + base_SPTrans * desconto_sptrans
        aux$tarifa_SPtrans_estudante <- aux$tarifa_SPtrans_base/2

        return(aux)
        }