#sourcing functions to process OD

source("901_convert_spatial.R")

source("902_distancia_viagem_centro.R")

source("903_flag_integration.R")

source("904_get_fare.R")

source("905_estimate_fare_unit.R")

source("906_estimate_fare.R")

source("907_get_trip_fare.R")

source("908_d_uteis_equivalentes.R")

source("909b_correcting_tariffs.R")

source("910a_tariff_values.R")
sp_fares <<- tariff_values()
source("910b_tarifas_EMTU.R")
EMTU_dados <<- tarifas_EMTU()
source("910c_tarifa_municips.R")
tarif_muni <<- tarifas_municips()